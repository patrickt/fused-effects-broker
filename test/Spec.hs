{-# LANGUAGE GADTs, TypeApplications, TypeFamilies #-}

module Main (main) where

import Control.Carrier.Broker.STM
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Monad.IO.Class
import Data.Kind (Type)

data PingPong = PingPong Int

data PMsg :: Type -> Type where
  Ping  :: PMsg ()
  Count :: TMVar Int -> PMsg Int

type instance Message PingPong = PMsg


main :: IO ()
main = runBroker (PingPong 0) $ do
  x <- register @PingPong $ \e -> do
    PingPong p <- ask
    case e of
      Ping -> pure (PingPong (p + 1))
      Count x -> do
        sendM (swapTMVar x p)
        pure (PingPong p)


  broadcast @PingPong x Ping
