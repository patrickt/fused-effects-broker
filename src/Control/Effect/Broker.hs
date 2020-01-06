{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables,
             TypeApplications, TypeFamilies, TypeFamilyDependencies, QuantifiedConstraints #-}

module Control.Effect.Broker
  ( Broker (..)
  , SomeMessage (..)
  , ActorM
  , register
  , ActorId
  , broadcast
  , message
  , messageSync
  ) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Kind (Type)

type ActorM r = ReaderC r (LiftC STM)

type ActorId = ThreadId

-- type family Message r :: Type -> Type

data SomeMessage msg = forall a . SomeMessage (msg a)

data Broker r (msg :: Type -> Type) m k
  = Register (SomeMessage msg -> ActorM r r) (ActorId -> m k)
  | Kill ActorId (m k)
  | Broadcast ActorId (msg ()) (m k)
  | forall a . Message ActorId (TMVar a -> msg a) (TMVar a -> m k)

instance HFunctor (Broker r msg) where
  hmap f (Register act k) = Register act (f . k)
  hmap f (Kill i k) = Kill i (f k)
  hmap f (Broadcast i m k) = Broadcast i m (f k)
  hmap f (Message i go k) = Message i go (f . k)

register :: forall r msg sig m . Has (Broker r msg) sig m => (SomeMessage msg -> ActorM r r) -> m ActorId
register act = send (Register act pure)


broadcast :: forall r msg sig m . Has (Broker r msg) sig m => ActorId -> msg () -> m ()
broadcast i m = send @(Broker r msg) (Broadcast i m (pure ()))

message :: forall r msg sig m a . Has (Broker r msg) sig m => ActorId -> (TMVar a -> msg a) -> m (TMVar a)
message i act = send @(Broker r msg) (Message i act pure)

messageSync :: forall r msg sig m a . (MonadIO m, Has (Broker r msg) sig m) => ActorId -> (TMVar a -> msg a) -> m a
messageSync i act = do
  item <- message @r i act
  liftIO (atomically (readTMVar item))

