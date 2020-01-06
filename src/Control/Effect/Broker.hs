{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TypeFamilies, TypeFamilyDependencies, RankNTypes, GADTs, ScopedTypeVariables, MultiParamTypeClasses #-}

module Control.Effect.Broker
  ( Broker (..)
  , SomeMessage (..)
  , ActorM
  , register
  , kill
  , ActorId
  , Message
  , broadcast
  , message
  , messageSync
  ) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Algebra
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Kind (Type)
import Control.Concurrent

type ActorM r = ReaderC r (LiftC STM)

type ActorId r = ThreadId

type family Message r :: Type -> Type

data SomeMessage r = forall a . SomeMessage (Message r a)

data Broker r m k
  = forall a . Register (Message r a -> ActorM r r) (ActorId r -> m k)
  | Kill (ActorId r) (m k)
  | Broadcast (ActorId r) (Message r ()) (m k)
  | forall a . Message (ActorId r) (TMVar a -> Message r a) (TMVar a -> m k)

instance HFunctor (Broker r) where
  hmap f (Register act k) = Register act (f . k)
  hmap f (Kill i k) = Kill i (f k)
  hmap f (Broadcast i m k) = Broadcast i m (f k)
  hmap f (Message i go k) = Message i go (f . k)

register :: forall r sig m a . Has (Broker r) sig m => (Message r a -> ActorM r r) -> m (ActorId r)
register act = send (Register act pure)

kill :: forall r sig m . Has (Broker r) sig m => ActorId r -> m ()
kill i = send @(Broker r) (Kill i (pure ()))

broadcast :: forall r sig m . Has (Broker r) sig m => ActorId r -> Message r () -> m ()
broadcast i m = send @(Broker r) (Broadcast i m (pure ()))

message :: forall r sig m a . (MonadIO m, Has (Broker r) sig m) => ActorId r -> (TMVar a -> Message r a) -> m (TMVar a)
message i act = send @(Broker r) (Message i act pure)

messageSync :: forall r sig m a . (MonadIO m, Has (Broker r) sig m) => ActorId r -> (TMVar a -> Message r a) -> m a
messageSync i act = do
  item <- message @r i act
  liftIO (atomically (readTMVar item))

