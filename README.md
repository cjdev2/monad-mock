# monad-mock [![Build Status](https://travis-ci.org/cjdev/monad-mock.svg?branch=master)](https://travis-ci.org/cjdev/monad-mock)

`monad-mock` is a Haskell package that provides a monad transformer to help create “mocks” of `mtl`-style typeclasses, intended for use in unit tests. A mock can be executed by providing a sequence of expected monadic calls and their results, and the mock will verify that the computation conforms to the expectation.

For example, imagine a `MonadFileSystem` typeclass, which describes a class of
monads that may perform filesystem operations:

```haskell
class Monad m => MonadFileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
```

Using `MockT`, it’s possible to test computations that use `MonadFileSystem`
in a completely pure way:

```haskell
copyFile :: MonadFileSystem m => FilePath -> FilePath -> m ()
copyFile a b = do
  x <- readFile a
  writeFile b x

makeMock "FileSystemAction" [ts| MonadFileSystem |]

spec = describe "copyFile" $
  it "reads a file and writes its contents to another file" $
    evaluate $ copyFile "foo.txt" "bar.txt"
      & runMock [ ReadFile "foo.txt" :-> "contents"
                , WriteFile "bar.txt" "contents" :-> () ]
```

For more information, [see the documentation on Hackage][monad-mock].

[monad-mock]: https://hackage.haskell.org/package/monad-mock
