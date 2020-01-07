{-# LANGUAGE BlockArguments, GADTs, StandaloneDeriving, TypeApplications, TypeFamilies #-}

module Main (main) where

import Control.Carrier.Broker.STM
import Control.Carrier.State.Strict
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
  Count :: PMsg Int

deriving instance Show (PMsg a)

data BlackBox = BlackBox String

data BMsg :: Type -> Type where
  Update :: String -> BMsg ()
  View :: BMsg String

main :: IO ()
main = runBroker @PingPong @PMsg $ runBroker @BlackBox @BMsg $ do
  [x, y] <- for [0..1] $ \n -> register (PingPong n) $ \e -> do
    PingPong p <- get
    case e of
      Ping  -> put (PingPong (p + 1))
      Count -> pure p

  sign <- register (BlackBox "hello") $ \e -> do
    BlackBox s <- get
    case e of
      Update new -> put (BlackBox new)
      View       -> pure s

  -- for_ [0..4] $ \_ -> do
  --   broadcast @PingPong x Ping
  --   message @PingPong x Count

  messageSync @PingPong y Count >>= liftIO . print

  -- broadcast @BlackBox sign (Update "world")
  -- messageSync @BlackBox sign View >>= liftIO . print
