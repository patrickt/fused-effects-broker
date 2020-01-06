{-# LANGUAGE GADTs, TypeApplications, TypeFamilies #-}

module Main (main) where

import Control.Carrier.Broker.STM
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Monad.IO.Class
import Data.Foldable
import Data.Kind (Type)
import Data.Traversable
import Debug.Trace

data PingPong = PingPong Int

data PMsg :: Type -> Type where
  Ping  :: PMsg ()
  Count :: TMVar Int -> PMsg Int

instance Show (PMsg a) where
  show Ping      = "ping"
  show (Count _) = "count"

main :: IO ()
main = runBroker @_ @PingPong @PMsg (PingPong 0) $ do
  [x, y] <- for [0..1] $ \n -> register (PingPong n) $ \(SomeMessage e) -> do
    PingPong p <- ask
    case e of
      Ping -> pure (PingPong (p + 1))
      Count x -> do
        sendM (putTMVar x p)
        pure (PingPong p)

  for_ [0..4] $ \_ -> do
    broadcast @PingPong x Ping
    messageSync @PingPong x Count >>= liftIO . print

  broadcast @PingPong y Ping
  messageSync @PingPong y Count >>= liftIO . print
