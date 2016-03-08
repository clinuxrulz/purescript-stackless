## Module Control.Monad.Stackless.Trans

#### `StacklessT`

``` purescript
newtype StacklessT m a
```

Stackless MonadRec Transformer

##### Instances
``` purescript
(Applicative m) => Functor (StacklessT m)
(Applicative m) => Apply (StacklessT m)
(Applicative m) => Applicative (StacklessT m)
(Applicative m) => Bind (StacklessT m)
(Applicative m) => Monad (StacklessT m)
MonadTrans StacklessT
(Applicative m) => MonadRec (StacklessT m)
(MonadError e m, MonadRec m) => MonadError e (StacklessT m)
(MonadReader r m, MonadRec m) => MonadReader r (StacklessT m)
(MonadWriter w m, MonadRec m) => MonadWriter w (StacklessT m)
(MonadState s m) => MonadState s (StacklessT m)
(MonadRWS r w s m, MonadRec m) => MonadRWS r w s (StacklessT m)
```

#### `runStacklessT`

``` purescript
runStacklessT :: forall m a. (MonadRec m) => StacklessT m a -> m a
```

Unboxes the transformer with tailRecM.


