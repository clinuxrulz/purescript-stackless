module Control.Monad.Stackless.Trans
  ( StacklessT()
  , runStacklessT
  ) where

import Prelude ( Unit, unit, ($), (<<<), class Functor, class Apply
               , class Applicative, class Bind, class Monad, map, apply, pure
               , bind, return, (<$>), (>>=)
               )
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans (class MonadTrans)
import Data.Either (Either(Left, Right))

data StacklessF a next
  = Suspend (Unit -> next)
  | Done a

newtype SuspT m a = SuspT (m (StacklessF a (SuspT m a)))

unSuspT :: forall m a. SuspT m a -> m (StacklessF a (SuspT m a))
unSuspT (SuspT a) = a

suspSuspend :: forall m a. (Applicative m) => (Unit -> SuspT m a) -> SuspT m a
suspSuspend thunk = SuspT $ return $ Suspend thunk

newtype StacklessT m a = StacklessT (forall r. (a -> SuspT m r) -> SuspT m r)

unStacklessT :: forall m a. StacklessT m a -> (forall r. (a -> SuspT m r) -> SuspT m r)
unStacklessT (StacklessT a) = a

runStacklessT :: forall m a. (MonadRec m) => StacklessT m a -> m a
runStacklessT (StacklessT a) = do
  x <- unSuspT $ a (SuspT <<< return <<< Done)
  tailRecM go x
  where
    go :: StacklessF a (SuspT m a) -> m (Either (StacklessF a (SuspT m a)) a)
    go (Suspend thunk) = Left <$> ((unSuspT <<< thunk) unit)
    go (Done a) = return $ Right a

instance functorStacklessT :: (Applicative m) => Functor (StacklessT m) where
  map f (StacklessT s) = StacklessT (\k -> suspSuspend (\_ -> s (k <<< f)))

instance applyStacklessT :: (Applicative m) => Apply (StacklessT m) where
  apply (StacklessT sf) (StacklessT sa) = StacklessT (\k -> suspSuspend (\_ -> sf (\f -> suspSuspend (\_ -> sa (k <<< f)))))

instance applicativeStacklessT :: (Applicative m) => Applicative (StacklessT m) where
  pure a = StacklessT (\k -> k a)

instance bindStacklessT :: (Applicative m) => Bind (StacklessT m) where
  bind (StacklessT sa) f = StacklessT (\k -> suspSuspend (\_ -> sa (\a -> suspSuspend (\_ -> ((unStacklessT <<< f) a) k))))

instance monadStacklessT :: (Applicative m) => Monad (StacklessT m)

instance monadTransStacklessT :: MonadTrans StacklessT where
  lift m =
    StacklessT (\k -> SuspT $ do
      a <- m
      unSuspT $ k a
    )
