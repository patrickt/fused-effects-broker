{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables,
             TypeApplications, TypeFamilies, TypeFamilyDependencies, QuantifiedConstraints #-}

module Control.Effect.Broker
  ( Broker (..)
  , SomeMessage (..)
  , ListenerM
  , register
  , ListenerId
  , broadcast
  , message
  , messageSync
  ) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Kind (Type)
import Control.Monad

-- Listeners have access to a context 'r' and an STM environment.
-- Listeners never update their context manually: each message handler
-- introduced with 'register' ends up returning a new 'r' value.
type ListenerM r = StateC r (LiftC STM)

-- Each actor is associated with one owner.
type ListenerId r = ThreadId

-- All messages are GADTs, to provide measures of type safety
-- when passing transactional variables across process bounds.
-- For this reason, we have to have an existential hider.
data SomeMessage msg = forall a . SomeMessage (msg a) (TMVar a)

-- The type of event broking interactions.
data Broker r (msg :: Type -> Type) m k
  = Register r (forall a . msg a -> ListenerM r a) (ListenerId r -> m k)
  | forall a . Message (ListenerId r) (msg a) (TMVar a -> m k)

instance HFunctor (Broker r msg) where
  hmap f (Register r act k) = Register r act (f . k)
  hmap f (Message i go k) = Message i go (f . k)

type (~>) a b = forall x . a x -> b x

-- Registering a new listening involves providing an initial actor state
-- and an ListenerM handler that will be run infinitely on that state.
-- Formally speaking,
register ::
  forall r msg sig m .
  ( Has (Broker r msg) sig m )
  => r
  -> (msg ~> ListenerM r)
  -> m (ListenerId r)
register initial act = send (Register initial act pure)

-- Communication across process boundaries involves communicating with TMVars.
-- To send a message, you provide a function that expects a TMVar and returns a message type.
-- A TMVar is allocated for you, passed into that function, and the resulting message is sent
-- across the process boundary.
--
-- It is your responsibility to ensure that handlers write to all TMVars from which results are required.
message :: forall r msg sig m a . Has (Broker r msg) sig m => ListenerId r -> msg a -> m (TMVar a)
message i act = send @(Broker r msg) (Message i act pure)

-- A helper for the simple case where we want to send a message and read from its result TMVar immediately.
messageSync :: forall r msg sig m a . (MonadIO m, Has (Broker r msg) sig m) => ListenerId r -> msg a -> m a
messageSync i act = do
  item <- message @r i act
  liftIO (atomically (readTMVar item))

-- A helper for messages that need no reply.
broadcast :: forall r msg sig m . Has (Broker r msg) sig m => ListenerId r -> msg () -> m ()
broadcast i = void . message @r i
