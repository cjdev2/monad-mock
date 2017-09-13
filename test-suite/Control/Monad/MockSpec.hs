{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.MockSpec (spec) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (evaluate)
import Control.Monad.Except (MonadError, runExcept)
import Data.Function ((&))
import Test.Hspec

import Control.Monad.Mock
import Control.Monad.Mock.TH

class MonadError e m => MonadFileSystem e m | m -> e where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
makeAction "FileSystemAction" [ts| MonadFileSystem String |]

copyFileAndReturn :: MonadFileSystem e m => FilePath -> FilePath -> m String
copyFileAndReturn a b = do
  x <- readFile a
  writeFile b x
  return x

spec :: Spec
spec = describe "MockT" $ do
  it "runs computations with mocked method implementations" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ ReadFile "foo.txt" :-> "file contents"
                     , WriteFile "bar.txt" "file contents" :-> () ]
          & runExcept
    result `shouldBe` Right "file contents"

  it "raises an exception if calls are not in the right order" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ WriteFile "bar.txt" "file contents" :-> ()
                     , ReadFile "foo.txt" :-> "file contents" ]
          & runExcept
        exnMessage =
          "runMockT: argument mismatch in readFile\n\
          \  given: ReadFile \"foo.txt\"\n\
          \  expected: WriteFile \"bar.txt\" \"file contents\"\n"
    evaluate result `shouldThrow` errorCall exnMessage

  it "raises an exception if calls are missing" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ ReadFile "foo.txt" :-> "file contents"
                     , WriteFile "bar.txt" "file contents" :-> ()
                     , ReadFile "qux.txt" :-> "file contents 2" ]
          & runExcept
        exnMessage =
          "runMockT: expected the following unexecuted actions to be run:\n\
          \  ReadFile \"qux.txt\"\n"
    evaluate result `shouldThrow` errorCall exnMessage

  it "raises an exception if there are too many calls" $ do
    let result = copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ ReadFile "foo.txt" :-> "file contents" ]
          & runExcept
        exnMessage =
          "runMockT: expected end of program, called writeFile\n\
          \  given action: WriteFile \"bar.txt\" \"file contents\"\n"
    evaluate result `shouldThrow` errorCall exnMessage
