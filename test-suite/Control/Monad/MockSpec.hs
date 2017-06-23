module Control.Monad.MockSpec where

import Prelude hiding (readFile, writeFile)

import Control.Exception (evaluate)
import Data.Function ((&))
import Data.Type.Equality ((:~:)(..))
import Test.Hspec

import Control.Monad.Mock

class Monad m => MonadFileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

data FileSystemAction r where
  ReadFile :: FilePath -> FileSystemAction String
  WriteFile :: FilePath -> String -> FileSystemAction ()
deriving instance Eq (FileSystemAction r)
deriving instance Show (FileSystemAction r)

instance Action FileSystemAction where
  eqAction (ReadFile a) (ReadFile b)
    = if a == b then Just Refl else Nothing
  eqAction (WriteFile a b) (WriteFile c d)
    = if a == c && b == d then Just Refl else Nothing
  eqAction _ _ = Nothing

instance Monad m => MonadFileSystem (MockT FileSystemAction m) where
  readFile a = mockAction "readFile" (ReadFile a)
  writeFile a b = mockAction "writeFile" (WriteFile a b)

copyFileAndReturn :: MonadFileSystem m => FilePath -> FilePath -> m String
copyFileAndReturn a b = do
  x <- readFile a
  writeFile b x
  return x

spec :: Spec
spec = describe "MockT" $ do
  it "runs computations with mocked method implementations" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMock [ ReadFile "foo.txt" :-> "file contents"
                    , WriteFile "bar.txt" "file contents" :-> () ]
    result `shouldBe` "file contents"

  it "raises an exception if calls are not in the right order" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMock [ WriteFile "bar.txt" "file contents" :-> ()
                    , ReadFile "foo.txt" :-> "file contents" ]
        exnMessage =
          "runMockT: argument mismatch in readFile\n\
          \  given: ReadFile \"foo.txt\"\n\
          \  expected: WriteFile \"bar.txt\" \"file contents\"\n"
    evaluate result `shouldThrow` errorCall exnMessage

  it "raises an exception if calls are missing" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMock [ ReadFile "foo.txt" :-> "file contents"
                    , WriteFile "bar.txt" "file contents" :-> ()
                    , ReadFile "qux.txt" :-> "file contents 2" ]
        exnMessage =
          "runMockT: expected the following unexecuted actions to be run:\n\
          \  ReadFile \"qux.txt\"\n"
    evaluate result `shouldThrow` errorCall exnMessage

  it "raises an exception if there are too many calls" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMock [ ReadFile "foo.txt" :-> "file contents" ]
        exnMessage =
          "runMockT: expected end of program, called writeFile\n\
          \  given action: WriteFile \"bar.txt\" \"file contents\"\n"
    evaluate result `shouldThrow` errorCall exnMessage
