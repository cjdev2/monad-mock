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
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State (MonadState)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl)
import Control.Monad.Writer (MonadWriter)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Type.Equality ((:~:)(..))

import Control.Monad.Mock (Action(..), MonadMock(..), WithResult(..))

type MockT f m = MockT_ (PrimState m) f m

type Mock s f = MockT f (ST s)

newtype MockT_ s f m a = MockT (ReaderT (MutVar s [WithResult f]) m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix
           , MonadState st, MonadCont, MonadError e, MonadWriter w
           , MonadCatch, MonadThrow, MonadMask
           , MonadBase b, MonadBaseControl b, MonadTransControl
           , PrimMonad)

instance MonadReader r m => MonadReader r (MockT_ s f m) where
  ask = lift ask
  local f (MockT act) = MockT $ do
    env <- ask
    lift $ local f $ runReaderT act env

runMockT :: forall f m a .
            (Action f, PrimMonad m) =>
            [WithResult f] -> MockT f m a -> m a
runMockT actions (MockT x) = do
  ref <- newMutVar actions
  r <- runReaderT x ref
  leftovers <- readMutVar ref
  case leftovers of
    [] -> return r
    remainingActions -> error'
      $ "runMockT: expected the following unexecuted actions to be run:\n"
      ++ unlines (map (\(action :-> _) -> "  " ++ showAction action) remainingActions)

runMock :: forall f a. Action f => [WithResult f] -> (forall s. Mock s f a) -> a
runMock actions x = runST $ runMockT actions x

instance (PrimMonad m, PrimState m ~ s) => MonadMock f (MockT_ s f m) where
  mockAction fnName action = MockT $ do
    ref <- ask
    results <- lift $ readMutVar ref
    case results of
      [] -> error'
        $ "runMockT: expected end of program, called " ++ fnName ++ "\n"
        ++ "  given action: " ++ showAction action ++ "\n"
      (action' :-> r) : actions
        | Just Refl <- action `eqAction` action' -> do
            lift $ writeMutVar ref actions
            return r
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
