{-# LANGUAGE GADTs, TypeApplications, TypeFamilies #-}

module Main (main) where

import Control.Carrier.Broker.STM
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Monad.IO.Class
import Data.Kind (Type)
import Debug.Trace

data PingPong = PingPong Int

data PMsg :: Type -> Type where
  Ping  :: PMsg ()
  Count :: TMVar Int -> PMsg Int

instance Show (PMsg a) where
  show Ping      = "ping"
  show (Count _) = "count"

main :: IO ()
main = runBroker @IO @PingPong @PMsg (PingPong 0) $ do
  x <- register @PingPong @PMsg $ \(SomeMessage e) -> do
    traceShowM e
    PingPong p <- ask
    case e of
      Ping -> pure (PingPong (p + 1))
      Count x -> do
        sendM (putTMVar x p)
        pure (PingPong p)

  broadcast @PingPong x Ping
  messageSync @PingPong x Count >>= liftIO . print
