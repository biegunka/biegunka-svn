{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Biegunka.Source.Svn.Internal
  ( svn
  , Svn
  , Repository
  , Config(..)
  , url
  , path
  , ignoreExternals
  , revision
  , update
  , defaultConfig
  , Err(..)
  , _ErrSvn
  , _ErrCustom
  ) where

import           Control.Lens (Prism', prism')
import           Control.Monad ((<=<), guard, when)
import           Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.List as List
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.String (IsString(fromString))
import           System.Directory (doesDirectoryExist)
import           System.Exit (ExitCode(..))
import qualified System.Process as P
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Control.Biegunka.Execute.Exception (sourceFailure)
import           Control.Biegunka.Language (Scope(Sources, Actions))
import           Control.Biegunka.Script (Script, sourced)


svn :: Svn Repository FilePath -> Script 'Actions () -> Script 'Sources ()
svn f actions =
  sourced "svn" configUrl configPath actions (update config)
 where
  config@Config { configUrl, configPath } =
    f defaultConfig

type Svn a b = Config () () -> Config a b

url :: Repository -> Config a b -> Config Repository b
url u config = config { configUrl = u }

path :: Repository -> Config a b -> Config a FilePath
path p config = config { configPath = p }

ignoreExternals :: Config a b -> Config a b
ignoreExternals config = config { configIgnoreExternals = True }

revision :: String -> Config a b -> Config a b
revision r config = config { configRevision = r }

type Repository = String

data Config a b = Config
  { configUrl             :: a
  , configPath            :: b
  , configIgnoreExternals :: Bool
  , configRevision        :: String
  } deriving (Show, Eq)

defaultConfig :: Config () ()
defaultConfig = Config
  { configUrl             = ()
  , configPath            = ()
  , configIgnoreExternals = False
  , configRevision        = "HEAD"
  }

class Update m where
  update :: Config Repository a -> FilePath -> m (Maybe String)

instance Update IO where
  update config =
    either (sourceFailure . errDisplay) return <=< runExceptT . update config

instance (e ~ Err, MonadIO m) => Update (ExceptT e m) where
  update config sourceRoot =
    liftIO (doesDirectoryExist sourceRoot) >>= \case
      False -> do
        _ <- svnCheckout config sourceRoot
        after <- revisionInfo sourceRoot
        return (pure (printf "‘none’ → ‘%s’" after))
      True -> do
        maybeUrl <- fmap parseSvnUrl (svnInfo sourceRoot)
        case maybeUrl of
          Nothing -> throwCustom "Path is a working copy, but `svn info` does not contain the URL"
          Just remoteUrl -> do
            when (remoteUrl /= configUrl config)
                 (throwCustom "Path is a working copy, but the URL is wrong")
            before <- revisionInfo sourceRoot
            _ <- svnUp config sourceRoot
            after <- revisionInfo sourceRoot
            return (printf "‘%s’ → ‘%s’" before after <$ guard (before /= after))
   where
    revisionInfo = fmap (maybe "unknown" show . parseSvnRevision) . svnInfo

errDisplay :: IsString s => Err -> s
errDisplay (ErrSvn ec err) = fromString (printf "`svn` exited with exit code %d: %s" err ec)
errDisplay (ErrCustom err) = fromString err

-- | Run @svn checkout@.
svnCheckout :: MonadIO m => Config Repository a -> FilePath -> ExceptT Err m Out
svnCheckout Config { configUrl, configIgnoreExternals, configRevision } fp =
  runSvn (["checkout", configUrl, "--revision", configRevision, fp] ++ ["--ignore-externals" | configIgnoreExternals]) Nothing

-- | Run @svn update@.
svnUp :: MonadIO m => Config a b -> FilePath -> ExceptT Err m Out
svnUp Config { configIgnoreExternals, configRevision } cwd =
  runSvn (["update", "--revision", configRevision] ++ ["--ignore-externals" | configIgnoreExternals]) (Just cwd)

-- | Run @svn info@.
svnInfo :: MonadIO m => FilePath -> ExceptT Err m Out
svnInfo cwd = runSvn ["info"] (Just cwd)

-- | Run an SVN command and return either non-zero exit code and
-- standard error contents or standard out contents.
runSvn :: MonadIO m => [String] -> Maybe FilePath -> ExceptT Err m Out
runSvn args cwd = ExceptT . liftIO $ do
  (ec, out, err) <- P.readCreateProcessWithExitCode proc ""
  return (exitCode (Left . ErrSvn err) (Right Out { out }) ec)
 where
  proc = (P.proc "svn" args) { P.cwd = cwd }

newtype Out = Out
  { out :: String
  } deriving (Show, Eq)

-- | The error is either a failed SVN command with its exit code and standard error
-- or a custom error message.
data Err = ErrSvn String Int | ErrCustom String
    deriving (Show, Eq)

_ErrSvn :: Prism' Err (String, Int)
_ErrSvn = prism' (uncurry ErrSvn) (\case ErrSvn string int -> Just (string, int); _ -> Nothing)

_ErrCustom :: Prism' Err String
_ErrCustom = prism' ErrCustom (\case ErrCustom string -> Just string; _ -> Nothing)

throwCustom :: Monad m => String -> ExceptT Err m a
throwCustom = throwE . ErrCustom

-- | Parse revision number from @svn info@ output.
parseSvnRevision :: Out -> Maybe Int
parseSvnRevision = readMaybe <=< parseInfo "Revision: "

-- | Parse URL from @svn info@ output.
parseSvnUrl :: Out -> Maybe String
parseSvnUrl = parseInfo "URL: "

-- | Parse revision number for @svn info@ output.
parseInfo :: String -> Out -> Maybe String
parseInfo prefix = listToMaybe . mapMaybe (List.stripPrefix prefix) . lines . out

-- | Eliminator for 'ExitCode'.
exitCode :: (Int -> a) -> a -> ExitCode -> a
exitCode f _ (ExitFailure n) = f n
exitCode _ z ExitSuccess = z
