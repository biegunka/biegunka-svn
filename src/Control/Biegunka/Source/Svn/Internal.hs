{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Biegunka.Source.Svn.Internal
  ( Svn
  , svn
  , Url
  , Config(..)
  , url
  , path
  , ignoreExternals
  , revision
  , ioUpdate
  , exceptUpdate
  , defaultConfig
  , Err(..)
  , _ErrSvn
  , _ErrCustom
  ) where

import           Control.Lens (Prism', prism')
import           Control.Monad ((<=<), guard)
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
import           Control.Biegunka.Language (Scope(Sources, Actions), Source(..))
import           Control.Biegunka.Script (Script, sourced)
import           Control.Biegunka.Source (Url, HasUrl(..), HasPath(..))


svn :: Svn Url FilePath -> Script 'Actions () -> Script 'Sources ()
svn f = sourced Source
  { sourceType   = "svn"
  , sourceFrom   = configUrl
  , sourceTo     = configPath
  , sourceUpdate = ioUpdate config
  }
 where
  config@Config { configUrl, configPath } =
    f defaultConfig

type Svn a b = Config () () -> Config a b

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

instance HasUrl (Config a b) (Config Url b) Url where
  url u config = config { configUrl = u }

instance HasPath (Config a b) (Config a FilePath) FilePath where
  path p config = config { configPath = p }

ignoreExternals :: Config a b -> Config a b
ignoreExternals config = config { configIgnoreExternals = True }

revision :: String -> Config a b -> Config a b
revision r config = config { configRevision = r }

ioUpdate :: Config Url a -> FilePath -> IO (Maybe String, IO (Maybe String))
ioUpdate config sourceRoot = toIo . (fmap . fmap) toIo $ exceptUpdate config sourceRoot

exceptUpdate :: MonadIO m => Config Url a -> FilePath -> ExceptT Err m (Maybe String, ExceptT Err m (Maybe String))
exceptUpdate config@Config { configUrl } sourceRoot =
  liftIO (doesDirectoryExist sourceRoot) >>= \case
    False -> return (pure "first checkout", finishCheckout config sourceRoot)
    True -> do
      maybeUrl <- fmap parseSvnUrl (svnInfo sourceRoot)
      case maybeUrl of
        Just remoteUrl
          | remoteUrl == configUrl -> do
            before <- revisionInfo sourceRoot
            after <- revisionInfo configUrl
            return (printf "‘%s’ → ‘%s’" before after <$ guard (before /= after), finishUpdate config sourceRoot)
          | otherwise -> throwCustom (printf "The working copy points to the wrong repository.\nExpected: ‘%s’\n But got: ‘%s’" remoteUrl configUrl)
        Nothing -> throwCustom "Path is a working copy, but ‘svn info’ does not contain the URL"

finishCheckout :: MonadIO m => Config Url a -> FilePath -> ExceptT Err m (Maybe String)
finishCheckout config sourceRoot = do
  _ <- svnCheckout config sourceRoot
  after <- revisionInfo sourceRoot
  return (pure (printf "‘none’ → ‘%s’" after))

finishUpdate :: MonadIO m => Config Url a -> FilePath -> ExceptT Err m (Maybe String)
finishUpdate config@Config { configUrl } sourceRoot = do
  before <- revisionInfo sourceRoot
  _ <- svnUp config sourceRoot
  after <- revisionInfo configUrl
  return (printf "‘%s’ → ‘%s’" before after <$ guard (before /= after))

revisionInfo :: MonadIO m => String -> ExceptT Err m String
revisionInfo = fmap (maybe "unknown" show . parseSvnRevision) . svnInfo

-- | Prettify and throw an 'Err' as a 'SourceException' exception.
toIo :: ExceptT Err IO c -> IO c
toIo = either (sourceFailure . errDisplay) return <=< runExceptT

errDisplay :: IsString s => Err -> s
errDisplay (ErrSvn ec err) = fromString (printf "`svn` exited with exit code %d: %s" err ec)
errDisplay (ErrCustom err) = fromString err

-- | Run @svn checkout@.
svnCheckout :: MonadIO m => Config Url a -> FilePath -> ExceptT Err m Out
svnCheckout Config { configUrl, configIgnoreExternals, configRevision } fp =
  runSvn (["checkout", configUrl, "--revision", configRevision, fp] ++ ["--ignore-externals" | configIgnoreExternals]) Nothing

-- | Run @svn update@.
svnUp :: MonadIO m => Config a b -> FilePath -> ExceptT Err m Out
svnUp Config { configIgnoreExternals, configRevision } cwd =
  runSvn (["update", "--revision", configRevision] ++ ["--ignore-externals" | configIgnoreExternals]) (Just cwd)

-- | Run @svn info@.
svnInfo :: MonadIO m => String -> ExceptT Err m Out
svnInfo repository = runSvn ["info", repository] Nothing

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
