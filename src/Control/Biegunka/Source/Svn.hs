{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Biegunka.Source.Svn
  ( svn
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


type Repository = String

svn :: Repository -> FilePath -> Script 'Actions () -> Script 'Sources ()
svn url fp actions =
  sourced "svn" url fp actions (updateSvn url)

newtype Out = Out
  { outStd :: String
  } deriving (Show, Eq)

data Err = Err
  { errExitCode :: Int
  , errStd      :: String
  } deriving (Show, Eq)

updateSvn :: Repository -> FilePath -> IO (Maybe String)
updateSvn url fp = do
  res <- runExceptT $
    liftIO (doesDirectoryExist fp) >>= \case
      True -> do
        before <- revision fp
        _ <- svnUp fp
        after <- revision fp
        return (bool (Just (printf "‘%s’ → ‘%s’" before after)) Nothing (before == after))
      False -> do
        _ <- svnCheckout url fp
        after <- revision fp
        return (Just (printf "checked ‘%s’ out" after))
  either (sourceFailure . errDisplay) return res
 where
  revision = fmap (maybe "unknown" show . parseRevision) . svnInfo

errDisplay :: IsString s => Err -> s
errDisplay Err { errExitCode, errStd } =
  fromString (printf "`svn` exited with exit code %d: %s" errExitCode errStd)

-- | Run @svn checkout@.
svnCheckout :: Repository -> FilePath -> ExceptT Err IO Out
svnCheckout uri fp = runSvn ["checkout", "--ignore-externals", uri, fp] Nothing

-- | Run @svn update@
svnUp :: FilePath -> ExceptT Err IO Out
svnUp cwd = runSvn ["update", "--ignore-externals"] (Just cwd)

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

-- | Parse revision number for @svn info@ output.
parseRevision :: Out -> Maybe Int
parseRevision = (readMaybe <=< listToMaybe) . mapMaybe (List.stripPrefix revisionPrefix) . lines . outStd
 where
  revisionPrefix = "Revision: "

-- | Eliminator for 'ExitCode'.
exitCode :: (Int -> a) -> a -> ExitCode -> a
exitCode f _ (ExitFailure n) = f n
exitCode _ z ExitSuccess = z
