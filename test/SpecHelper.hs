module SpecHelper
  ( svn
  , svnadmin
  ) where

import           Control.Exception (throwIO)
import           System.Exit (ExitCode(..))
import qualified System.Process as P
import           Text.Printf (printf)


svn :: [String] -> FilePath -> IO ()
svn = run "svn"

svnadmin :: [String] -> FilePath -> IO ()
svnadmin = run "svnadmin"

run :: FilePath -> [String] -> FilePath -> IO ()
run exe args cwd = do
  (ec, _out, err) <- P.readCreateProcessWithExitCode proc ""
  case ec of
    ExitSuccess -> return ()
    ExitFailure n -> let
        msg = printf "‘%s’ exited with exit code ‘%d’: %s" (unwords (exe : args)) n err
      in
        throwIO (userError msg)
 where
  proc = (P.proc exe args) { P.cwd = Just cwd }
