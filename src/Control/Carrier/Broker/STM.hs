{-# LANGUAGE DefaultSignatures, DerivingStrategies, ExistentialQuantification, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeApplications,
             TypeOperators, UndecidableInstances #-}

module Control.Carrier.Broker.STM
  ( BrokerC (..)
  , runBroker
  , module Control.Effect.Broker
  ) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBChan as Chan
import           Control.Effect.Broker
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Debug.Trace

data ChanConnection r msg = ChanConnection
  { sendChan :: Chan.TBChan (SomeMessage msg)
  , _self    :: ListenerId r
  }

data BrokerEnv r msg = BrokerEnv
  { clients :: TVar (Map (ListenerId r) (ChanConnection r msg))
  }

newtype BrokerC r msg m a = BrokerC (ReaderC (BrokerEnv r msg) m a)
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO)

brokerQueueSize :: Int
brokerQueueSize = 512

runBroker :: forall r msg m a . MonadIO m => BrokerC r msg m a -> m a
runBroker (BrokerC go) = do
  state <- liftIO $ newTVarIO Map.empty
  runReader (BrokerEnv state) go

instance forall r msg sig m . (MonadIO m, Algebra sig m) => Algebra (Broker r msg :+: sig) (BrokerC r msg m) where
  alg (L item) = do
    env <- BrokerC (ask @(BrokerEnv r msg))
    case item of
      Register initial act k -> k =<< liftIO (do
        queue <- Chan.newTBChanIO brokerQueueSize
        box   <- newTMVarIO initial
        self  <- forkIO . forever . atomically $ do
          SomeMessage msg receptacle <- Chan.readTBChan queue
          state <- takeTMVar box
          (new, result) <- runM . runState state . act $ msg
          putTMVar box new
          putTMVar receptacle result

        atomically . modifyTVar' (clients env) . Map.insert self $ ChanConnection queue self
        pure self)
      Message i reply k -> k =<< liftIO (atomically $ do
        traceM "processing message"
        members   <- readTVar (clients env)
        result    <- newEmptyTMVar
        case Map.lookup i members of
          Nothing -> pure ()
          Just x  -> Chan.writeTBChan (sendChan x) (SomeMessage reply result)
        pure result)

  alg (R other) = BrokerC (alg (R (handleCoercible other)))
