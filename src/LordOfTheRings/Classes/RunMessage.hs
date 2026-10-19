module LordOfTheRings.Classes.RunMessage (module LordOfTheRings.Classes.RunMessage) where

import LordOfTheRings.Classes.Entity
import LordOfTheRings.GameT
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Queue

-- | Each card, and everything else holding state, runs a message with a queue of its own:
-- what it pushes keeps the order it is written in, and goes on the front of the game's
-- queue once it is done (see 'LordOfTheRings.Classes.HasQueue.runQueueT'). A card falls
-- through to its attrs with @runMessage msg attrs@, which shares that queue, so its
-- messages and the attrs' go on in the order they were pushed.
class RunMessage a where
  runMessage :: (HasCallStack) => Message -> a -> QueueT Message GameT a
  default runMessage
    :: (HasCallStack, Entity a, RunMessage (EntityAttrs a)) => Message -> a -> QueueT Message GameT a
  runMessage msg = overAttrsM (runMessage msg)
