module Control.Monad.Stackless.Trans
  ( StacklessT()
  , runStacklessT
  ) where

import Control.Monad.Suspender.Trans (SuspenderT, runSuspenderT)
import Control.Monad.Suspender.Class (suspend)

import Prelude ( ($), (<<<), class Functor, class Apply, class Applicative
               , class Bind, class Monad, map, apply, pure, bind
               )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans (class MonadTrans, lift)

newtype StacklessT m a = StacklessT (SuspenderT m a)

unStacklessT :: forall m a. StacklessT m a -> SuspenderT m a
unStacklessT (StacklessT a) = a

runStacklessT :: forall m a. (MonadRec m) => StacklessT m a -> m a
runStacklessT (StacklessT a) = runSuspenderT a

instance functorStacklessT :: (Applicative m) => Functor (StacklessT m) where
  map f (StacklessT m) = StacklessT $ suspend (\_ -> map f m)

instance applyStacklessT :: (Applicative m) => Apply (StacklessT m) where
  apply (StacklessT mf) (StacklessT ma) = StacklessT $ suspend (\_ -> apply mf ma)

instance applicativeStacklessT :: (Applicative m) => Applicative (StacklessT m) where
  pure a = StacklessT $ pure a

instance bindStacklessT :: (Applicative m) => Bind (StacklessT m) where
  bind (StacklessT ma) f = StacklessT $ suspend (\_ -> bind ma (unStacklessT <<< f))

instance monadStacklessT :: (Applicative m) => Monad (StacklessT m)

instance monadTransT :: (Applicative m) => MonadTrans StacklessT where
  lift m = StacklessT $ lift m
