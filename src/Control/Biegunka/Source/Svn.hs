{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Biegunka.Source.Svn
  ( svn
  , Svn
  , Repository
  , Config
  , url
  , path
  , ignoreExternals
  ) where

import           Control.Monad ((<=<))
import           Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool (bool)
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
  sourced "svn" configUrl configPath actions (updateSvn config)
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

type Repository = String

data Config a b = Config
  { configUrl             :: a
  , configPath            :: b
  , configIgnoreExternals :: Bool
  } deriving (Show, Eq)

defaultConfig :: Config () ()
defaultConfig = Config
  { configUrl             = ()
  , configPath            = ()
  , configIgnoreExternals = False
  }

updateSvn :: Config Repository a -> FilePath -> IO (Maybe String)
updateSvn config fp = do
  res <- runExceptT $
    liftIO (doesDirectoryExist fp) >>= \case
      True -> do
        before <- revision fp
        _ <- svnUp config fp
        after <- revision fp
        return (bool (Just (printf "‘%s’ → ‘%s’" before after)) Nothing (before == after))
      False -> do
        _ <- svnCheckout config fp
        after <- revision fp
        return (Just (printf "checked ‘%s’ out" after))
  either (sourceFailure . errDisplay) return res
 where
  revision = fmap (maybe "unknown" show . parseRevision) . svnInfo

errDisplay :: IsString s => Err -> s
errDisplay Err { errExitCode, errStd } =
  fromString (printf "`svn` exited with exit code %d: %s" errExitCode errStd)

-- | Run @svn checkout@.
svnCheckout :: Config Repository a -> FilePath -> ExceptT Err IO Out
svnCheckout Config { configUrl, configIgnoreExternals } fp =
  runSvn (["checkout", configUrl, fp] ++ ["--ignore-externals" | configIgnoreExternals]) Nothing

-- | Run @svn update@
svnUp :: Config a b -> FilePath -> ExceptT Err IO Out
svnUp Config { configIgnoreExternals } cwd =
  runSvn ("update" : ["--ignore-externals" | configIgnoreExternals]) (Just cwd)

-- | Run @svn info@.
svnInfo :: FilePath -> ExceptT Err IO Out
svnInfo cwd = runSvn ["info"] (Just cwd)

-- | Run an SVN command and return either non-zero exit code and
-- standard error contents or standard out contents.
runSvn :: [String] -> Maybe FilePath -> ExceptT Err IO Out
runSvn args cwd = ExceptT $ do
  (ec, outStd, errStd) <- P.readCreateProcessWithExitCode proc ""
  return (exitCode (\errExitCode -> Left Err { errExitCode, errStd }) (Right Out { outStd }) ec)
 where
  proc = (P.proc "svn" args) { P.cwd = cwd }

newtype Out = Out
  { outStd :: String
  } deriving (Show, Eq)

data Err = Err
  { errExitCode :: Int
  , errStd      :: String
  } deriving (Show, Eq)

-- | Parse revision number for @svn info@ output.
parseRevision :: Out -> Maybe Int
parseRevision = (readMaybe <=< listToMaybe) . mapMaybe (List.stripPrefix revisionPrefix) . lines . outStd
 where
  revisionPrefix = "Revision: "

-- | Eliminator for 'ExitCode'.
exitCode :: (Int -> a) -> a -> ExitCode -> a
exitCode f _ (ExitFailure n) = f n
exitCode _ z ExitSuccess = z
