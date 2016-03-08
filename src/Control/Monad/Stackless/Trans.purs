module Control.Monad.Stackless.Trans
  ( StacklessT()
  , runStacklessT
  ) where

import Prelude ( Unit, unit, ($), (<<<), class Functor, class Apply
               , class Applicative, class Bind, class Monad, bind, return
               , (<$>), (>>=)
               )
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Reader.Class (class MonadReader, ask, local)
import Control.Monad.Writer.Class (class MonadWriter, pass, listen, writer)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.RWS.Class (class MonadRWS)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans (class MonadTrans, lift)
import Data.Either (Either(Left, Right), either)

data StacklessF a next
  = Suspend (Unit -> next)
  | Done a

newtype SuspT m a = SuspT (m (StacklessF a (SuspT m a)))

unSuspT :: forall m a. SuspT m a -> m (StacklessF a (SuspT m a))
unSuspT (SuspT a) = a

suspSuspend :: forall m a. (Applicative m) => (Unit -> SuspT m a) -> SuspT m a
suspSuspend thunk = SuspT $ return $ Suspend thunk

-- | Stackless MonadRec Transformer
newtype StacklessT m a = StacklessT (forall r. (a -> SuspT m r) -> SuspT m r)

unStacklessT :: forall m a. StacklessT m a -> (forall r. (a -> SuspT m r) -> SuspT m r)
unStacklessT (StacklessT a) = a

-- | Unboxes the transformer with tailRecM.
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

instance monadRecStacklessT :: (Applicative m) => MonadRec (StacklessT m) where
  tailRecM f a = f a >>= (either (\a' -> tailRecM f a') return)

instance monadErrorStacklessT :: (MonadError e m, MonadRec m) => MonadError e (StacklessT m) where
  throwError e = lift $ throwError e
  catchError m f =
    StacklessT (\k -> SuspT $ do
      (catchError
        (Right <$> runStacklessT m)
        (\err -> return $ Left $ f err)
      ) >>= (
        either
          (\x -> unSuspT $ (unStacklessT x) k)
          (unSuspT <<< k)
      )
    )

instance monadReaderStacklessT :: (MonadReader r m, MonadRec m) => MonadReader r (StacklessT m) where
  local f m = lift $ local f (runStacklessT m)
  ask = lift $ ask

instance monadWriterStacklessT :: (MonadWriter w m, MonadRec m) => MonadWriter w (StacklessT m) where
  pass m = lift $ pass (runStacklessT m)
  listen m = lift $ listen (runStacklessT m)
  writer a = lift $ writer a

instance monadStateStacklessT :: (MonadState s m) => MonadState s (StacklessT m) where
  state f = lift $ state f

instance monadRWSStacklessT :: (MonadRWS r w s m, MonadRec m) => MonadRWS r w s (StacklessT m)

instance monadEffStacklessT :: (MonadEff e m) => MonadEff e (StacklessT m) where
  liftEff m = lift $ liftEff m
