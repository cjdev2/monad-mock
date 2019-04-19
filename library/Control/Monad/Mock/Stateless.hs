{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A version of 'MockT' with a stateless 'MonadTransControl' instance
module Control.Monad.Mock.Stateless
  (
  -- * The MonadMock class
    MonadMock(..)

  -- * The MockT monad transformer
  , MockT
  , Mock
  , runMockT
  , runMock
  , MockT_

  -- * Actions and actions with results
  , Action(..)
  , WithResult(..)
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State (MonadState)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Control
import Control.Monad.Writer (MonadWriter)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Type.Equality ((:~:)(..))

import Control.Monad.Mock (Action(..), MonadMock(..))

type MockT f m = MockT_ (PrimState m) f m m

type Mock s f = MockT f (ST s)

-- | Represents both an expected call (an 'Action') and its expected result.
data WithResult m f where
  -- | Matches a specific command
  (:->)     :: f r -> m r -> WithResult m f
  -- | Skips commands as long as the predicate returns something
  SkipWhile :: (forall r. f r -> Maybe (m r)) -> WithResult m f

newtype MockT_ s f n m a = MockT (ReaderT (MutVar s [WithResult n f]) m a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadFix
           , MonadState st, MonadCont, MonadError e, MonadWriter w
           , MonadCatch, MonadThrow, MonadMask
           , MonadTrans, MonadTransControl
           , MonadBase b, MonadBaseControl b
           , PrimMonad)

instance MonadReader r m => MonadReader r (MockT_ s f n m) where
  ask = lift ask
  local f (MockT act) = MockT $ do
    env <- ask
    lift $ local f $ runReaderT act env

runMockT :: forall f m a .
            (Action f, PrimMonad m) =>
            [WithResult m f] -> MockT f m a -> m a
runMockT actions (MockT x) = do
  ref <- newMutVar actions
  r <- runReaderT x ref
  leftovers <- readMutVar ref
  case leftovers of
    [] -> return r
    remainingActions -> error'
      $ "runMockT: expected the following unexecuted actions to be run:\n"
      ++ unlines (map (\(action :-> _) -> "  " ++ showAction action) remainingActions)

runMock :: forall f a. Action f => [WithResult Identity f] -> (forall s. Mock s f a) -> a
runMock actions x = runST $ runMockT (map (\(a :-> b) -> a :-> return(runIdentity b)) actions) x

instance (PrimMonad m, PrimState m ~ s) => MonadMock f (MockT_ s f m m) where
  mockAction fnName action = do
    ref <- MockT ask
    results <- lift $ readMutVar ref
    case results of
      [] -> error'
        $ "runMockT: expected end of program, called " ++ fnName ++ "\n"
        ++ "  given action: " ++ showAction action ++ "\n"
      SkipWhile f : actions
        | Just res <- f action
        -> lift res
        | otherwise -> do
            lift $ writeMutVar ref actions
            mockAction fnName action
      (action' :-> r) : actions
        | Just Refl <- action `eqAction` action' -> do
            lift $ writeMutVar ref actions
            lift r
        | otherwise -> error'
            $ "runMockT: argument mismatch in " ++ fnName ++ "\n"
            ++ "  given: " ++ showAction action ++ "\n"
            ++ "  expected: " ++ showAction action' ++ "\n"


error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif
