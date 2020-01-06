{-# LANGUAGE BlockArguments, GADTs, TypeApplications, TypeFamilies #-}

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

data BlackBox = BlackBox String

data BMsg :: Type -> Type where
  Update :: String -> BMsg ()
  View :: TMVar String -> BMsg String

main :: IO ()
main = runBroker @PingPong @PMsg $ runBroker @BlackBox @BMsg $ do
  [x, y] <- for [0..1] $ \n -> register (PingPong n) $ \(SomeMessage e) -> do
    PingPong p <- ask
    case e of
      Ping -> pure (PingPong (p + 1))
      Count x -> do
        sendM (putTMVar x p)
        pure (PingPong p)

  sign <- register (BlackBox "hello") $ \(SomeMessage e) -> do
    BlackBox s <- ask
    case e of
      Update new -> pure (BlackBox new)
      View t -> do
        sendM (putTMVar t s)
        ask

  for_ [0..4] $ \_ -> do
    broadcast @PingPong x Ping
    messageSync @PingPong x Count >>= liftIO . print

  broadcast @PingPong y Ping
  messageSync @PingPong y Count >>= liftIO . print

  broadcast @BlackBox sign (Update "world")
  messageSync @BlackBox sign View >>= liftIO . print
