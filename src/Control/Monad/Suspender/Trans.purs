module Control.Monad.Suspender.Trans
  ( SuspenderT()
  , runSuspenderT
  ) where

import Control.Monad.Suspender.Class (class MonadSuspender)

import Prelude ( Unit, unit, ($), (<<<), class Functor, class Apply
               , class Applicative, class Bind, class Monad, map, apply, pure
               , bind, return, (<$>), (>>=)
               )
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans (class MonadTrans)
import Data.Either (Either(Left, Right))

data SuspenderF a next
  = Suspend (Unit -> next)
  | Done a

newtype SuspT m a = SuspT (m (SuspenderF a (SuspT m a)))

unSuspT :: forall m a. SuspT m a -> m (SuspenderF a (SuspT m a))
unSuspT (SuspT a) = a

suspTBind :: forall m a b. (Applicative m) => SuspT m a -> (a -> SuspT m b) -> SuspT m b
suspTBind (SuspT m) f = SuspT $ (\x -> Suspend (\_ -> go x)) <$> m
  where
    go :: SuspenderF a (SuspT m a) -> SuspT m b
    go (Suspend thunk) = SuspT $ return $ Suspend (\_ -> suspTBind (thunk unit) f)
    go (Done a) = f a

newtype Codensity m a = Codensity (forall r. (a -> m r) -> m r)

runCodensity :: forall m a r. Codensity m a -> (a -> m r) -> m r
runCodensity (Codensity c) = c

instance functorCodensity :: Functor (Codensity m) where
  map f (Codensity c) = Codensity (\k -> c (k <<< f))

instance applyCodensity :: Apply (Codensity m) where
  apply (Codensity cf) (Codensity ca) = Codensity (\k -> cf (\f -> ca (k <<< f)))

instance applicativeCodensity :: Applicative (Codensity m) where
  pure a = Codensity (\k -> k a)

instance bindCodensity :: Bind (Codensity m) where
  bind (Codensity ca) f = Codensity (\k -> ca (\a -> (runCodensity <<< f) a k))

instance monadCodensity :: Monad (Codensity m)

instance monadTransCodensity :: MonadTrans Codensity where
  lift m = Codensity (m >>= _)

newtype SuspenderT m a = SuspenderT (Codensity (SuspT m) a)

unSuspenderT :: forall m a. SuspenderT m a -> Codensity (SuspT m) a
unSuspenderT (SuspenderT a) = a

runSuspenderT :: forall m a. (MonadRec m) => SuspenderT m a -> m a
runSuspenderT (SuspenderT a) = do
  x <- unSuspT $ runCodensity a (SuspT <<< return <<< Done)
  tailRecM go x
  where
    go :: SuspenderF a (SuspT m a) -> m (Either (SuspenderF a (SuspT m a)) a)
    go (Suspend thunk) = Left <$> ((unSuspT <<< thunk) unit)
    go (Done a) = return $ Right a

instance functorSuspenderT :: Functor (SuspenderT m) where
  map f (SuspenderT s) = SuspenderT $ map f s

instance applySuspenderT :: Apply (SuspenderT m) where
  apply (SuspenderT sf) (SuspenderT sa) = SuspenderT $ apply sf sa

instance applicativeSuspenderT :: Applicative (SuspenderT m) where
  pure a = SuspenderT $ pure a

instance bindSuspenderT :: Bind (SuspenderT m) where
  bind (SuspenderT sa) f = SuspenderT $ bind sa (unSuspenderT <<< f)

instance monadSuspenderT :: Monad (SuspenderT m)

codSuspend :: forall m a. (Applicative m) => (Unit -> SuspT m a) -> Codensity (SuspT m) a
codSuspend thunk = Codensity (\k -> suspTBind (SuspT $ return $ Suspend thunk) k)

instance monadSuspenderSuspenderT :: (Applicative m) => MonadSuspender (SuspenderT m) where
  suspend thunk = SuspenderT $ codSuspend (\_ -> runCodensity (unSuspenderT $ thunk unit) (SuspT <<< return <<< Done))

instance monadTransSuspenderT :: MonadTrans SuspenderT where
  lift m = SuspenderT $ Codensity (\k -> suspTBind (SuspT $ Done <$> m) k)
