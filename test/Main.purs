module Test.Main where

import Prelude (Unit, unit, show, bind, return, (==), (<), (+), (-), (*), (/), ($), (++))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Stackless.Trans (StacklessT, runStacklessT)
import Control.Monad.Suspender.Trans (SuspenderT, runSuspenderT)
import Control.Monad.Suspender.Class (suspend)
import Control.Monad.Trans (lift)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  test1

test1 :: forall eff. Eff (console :: CONSOLE | eff) Unit
test1 = do
  result <- runSuspenderT $ go 0 0
  log $ "result = " ++ (show result)
  where
    go :: forall m. (MonadEff (console :: CONSOLE | eff) m) => Int -> Int -> SuspenderT m Int
    go n acc = do
      if n - (n / 10) * 10 == 0
        then do
          lift $ liftEff $ log $ show n
        else return unit
      if n < 1000000
        then do
          suspend (\_ -> go (n + 1) (acc + n))
        else do
          return $ acc + n
