module Control.Biegunka.Source.SvnSpec (spec) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.List.Lens (suffixed)
import           System.Directory (createDirectory)
import           System.FilePath (combine, addExtension)
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Hspec.Lens

import           Control.Biegunka.Source.Svn.Internal hiding (svn)
import qualified Control.Biegunka.Source.Svn.Internal as Svn
import           SpecHelper (svn, svnadmin)


spec :: Spec
spec =
  around (withSystemTempDirectory "biegunka-svn") $
    describe "update" $ do
      let fauxHelloWorld fauxDir = do
            writeFile (combine fauxDir "test") "hello\n"
            addEverythingAndCommit fauxDir
            appendFile (combine fauxDir "test") "world\n"
            addEverythingAndCommit fauxDir
            appendFile (combine fauxDir "test") "!\n"
            addEverythingAndCommit fauxDir

      context "when the checkout does not exist" $ do
        context "when the revision of the checkout is not specified" $
          it "creates a checkout at HEAD" $ \tmpDir -> do
            faux <- buildFaux tmpDir "faux" fauxHelloWorld
            result <- updateWorkcopy (local faux) tmpDir
            result `shouldHave` _Right._Just.only "‘none’ → ‘3’"

        context "when the revision of the checkout is specified" $
          it "creates a checkout at the specified revision" $ \tmpDir -> do
            faux <- buildFaux tmpDir "faux" fauxHelloWorld
            result <- updateWorkcopy (local faux . revision "1") tmpDir
            result `shouldHave` _Right._Just.only "‘none’ → ‘1’"

      context "when the checkout exists" $ do
        context "when there are changes in the remote repository" $
          it "updates the checkout" $ \tmpDir -> do
            faux <- buildFaux tmpDir "faux" fauxHelloWorld
            _ <- updateWorkcopy (local faux . revision "1") tmpDir
            result <- updateWorkcopy (local faux) tmpDir
            result `shouldHave` _Right._Just.only "‘1’ → ‘3’"

        context "when there are no changes in the remote repository" $
          it "does not register any" $ \tmpDir -> do
            faux <- buildFaux tmpDir "faux" fauxHelloWorld
            _ <- updateWorkcopy (local faux) tmpDir
            result <- updateWorkcopy (local faux) tmpDir
            result `shouldHave` _Right._Nothing

        context "when attempting to update from the wrong repository" $
          it "responds with an error" $ \tmpDir -> do
            faux <- buildFaux tmpDir "faux" fauxHelloWorld
            _ <- updateWorkcopy (local faux . revision "1") tmpDir
            result <- updateWorkcopy (Svn.url "http://example.com") tmpDir
            result `shouldHave` _Left._ErrCustom.suffixed "the URL is wrong"

        context "when it's bogus" $
          it "responds with an error" $ \tmpDir -> do
            createDirectory (combine tmpDir "workcopy")
            result <- updateWorkcopy (Svn.url "http://example.com") tmpDir
            result `shouldHave` _Left._ErrSvn._1.suffixed "is not a working copy\n"

buildFaux :: FilePath -> String -> (FilePath -> IO a) -> IO FilePath
buildFaux root prefix withFauxWorkcopy = do
  let fauxRepository = combine root prefix `addExtension` "repository"
      fauxWorkcopy = combine root prefix `addExtension` "workcopy"
  svnadmin ["create", fauxRepository] root
  svn ["checkout", "file://" ++  fauxRepository, fauxWorkcopy] root
  _ <- withFauxWorkcopy fauxWorkcopy
  return fauxRepository

updateWorkcopy :: MonadIO m => Svn Svn.Repository a -> FilePath -> m (Either Svn.Err (Maybe String))
updateWorkcopy f tmpDir =
  runExceptT (update (f defaultConfig) (combine tmpDir "workcopy"))

local :: FilePath -> Svn.Config a b -> Svn.Config Svn.Repository b
local fp = Svn.url ("file://" ++ fp)

addEverythingAndCommit :: FilePath -> IO ()
addEverythingAndCommit workcopy = () <$ do
  svn ["add", "--force", "."] workcopy
  svn ["commit", "-m", "commit message"] workcopy
  svn ["update"] workcopy
