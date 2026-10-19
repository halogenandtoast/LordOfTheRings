module LordOfTheRings.Message.Lifted.Queue (module LordOfTheRings.Message.Lifted.Queue) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Frontend
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Queue

-- | A monad whose pushes keep the order they are written in: a 'QueueT' collecting
-- messages for its caller. The helpers in "LordOfTheRings.Message.Lifted" ask for it, so
-- they cannot be used where 'push' would put each message in front of the last.
class (HasGame m, HasQueue Message m, MonadRandom m, Frontend m) => ReverseQueue m

instance (HasGame m, MonadIO m, MonadRandom m, Frontend m) => ReverseQueue (QueueT Message m)

-- | The messages an action pushes, in order, without pushing them.
capture :: (MonadIO m) => QueueT Message m () -> m [Message]
capture = evalQueueT
