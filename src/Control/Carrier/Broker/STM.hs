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
import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar
import           Control.Effect.Broker
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Unsafe.Coerce



data ChanConnection r msg = ChanConnection
  { sendChan :: Chan.InChan (SomeMessage msg)
  , recvChan :: Chan.OutChan (SomeMessage msg)
  , self     :: ActorId
  }

data BrokerEnv r msg = BrokerEnv
  { clients :: TVar (Map ActorId (ChanConnection r msg))
  }

newtype BrokerC r msg m a = BrokerC (ReaderC (BrokerEnv r msg) m a)
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO)

runBroker :: MonadIO m => r -> BrokerC r msg m a -> m a
runBroker r (BrokerC go) = do
  state <- liftIO $ newTVarIO Map.empty
  ctx <- liftIO $ newTVarIO r
  runReader (BrokerEnv state) go

instance forall r msg sig m . (MonadIO m, Algebra sig m) => Algebra (Broker r msg :+: sig) (BrokerC r msg m) where
  alg (L item) = do
    env <- BrokerC (ask @(BrokerEnv r msg))
    case item of
      Register initial act k -> do
        boxed <- liftIO $ newTVarIO initial
        (inch, ouch) <- liftIO $ Chan.newChan 512
        self <- liftIO . forkIO . forever $ do
          msg <- Chan.readChan ouch
          atomically $ do
            v <- readTVar boxed
            result <- runM $ runReader v (act msg)
            writeTVar boxed result

        let conn = ChanConnection inch ouch self
        liftIO . atomically . modifyTVar' (clients env) . Map.insert self $ conn
        k self
      Broadcast i m k -> do
        members <- liftIO . readTVarIO . clients $ env
        liftIO $ case Map.lookup i members of
          Nothing -> putStrLn "Warning: Couldn't find item to which to broadcast"
          Just x  -> Chan.writeChan (sendChan x) (SomeMessage m)
        k
      Message i reply k -> do
        container <- liftIO . atomically $ newEmptyTMVar
        members <- liftIO . readTVarIO . clients $ env
        liftIO $ case Map.lookup i members of
          Nothing -> putStrLn "Warning: Couldn't find item to which to broadcast"
          Just x  -> Chan.writeChan (sendChan x) (SomeMessage (reply container))
        k container

  alg (R other) = BrokerC (alg (R (handleCoercible other)))
