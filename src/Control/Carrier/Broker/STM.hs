{-# LANGUAGE DefaultSignatures, DerivingStrategies, ExistentialQuantification, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeApplications,
             TypeOperators, UndecidableInstances #-}

module Control.Carrier.Broker.STM
  ( BrokerC (..)
  , runBroker
  , module Control.Effect.Broker
  ) where

import           Control.Algebra
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

data ChanConnection r = ChanConnection
  { sendChan :: Chan.InChan (SomeMessage r)
  , recvChan :: Chan.OutChan (SomeMessage r)
  , self     :: ActorId r
  }

data BrokerEnv r = BrokerEnv
  { clients :: TVar (Map (ActorId r) (ChanConnection r))
  , context :: TVar r
  }

newtype BrokerC r m a = BrokerC (ReaderC (BrokerEnv r) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runBroker :: MonadIO m => r -> BrokerC r m a -> m a
runBroker r (BrokerC go) = do
  state <- liftIO $ newTVarIO Map.empty
  ctx <- liftIO $ newTVarIO r
  runReader (BrokerEnv state ctx) go

instance forall r sig m . (MonadIO m, Algebra sig m) => Algebra (Broker r :+: sig) (BrokerC r m) where
  alg (L item) = do
    env <- BrokerC (ask @(BrokerEnv r))
    case item of
      Register act k -> do
        (inch, ouch) <- liftIO $ Chan.newChan 512
        self <- liftIO . forkIO . forever $ do
          (SomeMessage item) <- Chan.readChan ouch
          atomically $ do
            v <- readTVar (context env)
            result <- runReader act item
            writeTVar (context env) result

        let conn = ChanConnection inch ouch self
        liftIO . atomically . modifyTVar' (clients env) . Map.insert self $ conn
        k self
      Broadcast i m k -> do
        members <- liftIO . readTVarIO . clients $ env
        liftIO $ case Map.lookup i members of
          Nothing -> putStrLn "Warning: Couldn't find item to which to broadcast"
          Just x  -> Chan.writeChan (sendChan x) (SomeMessage m :: SomeMessage r)
        k
      Message i reply k -> do
        container <- liftIO . atomically $ newEmptyTMVar
        members <- liftIO . readTVarIO . clients $ env
        liftIO $ case Map.lookup i members of
          Nothing -> putStrLn "Warning: Couldn't find item to which to broadcast"
          Just x  -> do
            Chan.writeChan (sendChan x) (SomeMessage (reply container) :: SomeMessage r)
        k container

  alg (R other) = BrokerC (alg (R (handleCoercible other)))
