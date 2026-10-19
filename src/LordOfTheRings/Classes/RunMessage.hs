module LordOfTheRings.Classes.RunMessage (module LordOfTheRings.Classes.RunMessage) where

import LordOfTheRings.Classes.Entity
import LordOfTheRings.GameT
import LordOfTheRings.Message
import LordOfTheRings.Prelude

class RunMessage a where
  runMessage :: (HasCallStack) => Message -> a -> GameT a
  default runMessage
    :: (HasCallStack, Entity a, RunMessage (EntityAttrs a)) => Message -> a -> GameT a
  runMessage msg = overAttrsM (runMessage msg)

liftRunMessage :: (HasCallStack, MonadTrans t, RunMessage a) => Message -> a -> t GameT a
liftRunMessage msg = lift . runMessage msg
