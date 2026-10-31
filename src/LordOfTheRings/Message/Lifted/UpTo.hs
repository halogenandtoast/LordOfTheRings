{-# LANGUAGE RankNTypes #-}

-- | "Choose up to X": the player picks the options one by one, each at most once, until
-- X are chosen or they are done, as the Dream-chaser campaign's scalable boons read.
module LordOfTheRings.Message.Lifted.UpTo (UpTo (..), chooseUpToM) where

import LordOfTheRings.Card (Card)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Message.Lifted.Choose
import LordOfTheRings.Message.Lifted.Queue (ReverseQueue)
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Target)

-- | An option: named by a key under @label.@, or chosen by clicking its target.
data UpTo
  = UpToLabel Text (forall n. ReverseQueue n => n ())
  | UpToTarget Target (forall n. ReverseQueue n => n ())

-- | Each option chosen resolves, then the player may choose another of those after it: the
-- options come as a set, the order they are listed in kept.
chooseUpToM :: ReverseQueue m => PlayerId -> Text -> Target -> [Card] -> Int -> [UpTo] -> m ()
chooseUpToM pid key source shown n opts = when (n > 0 && notNull opts) $ chooseOneM pid do
  questionLabeled key
  causedBy source
  showing shown
  labeled "done" (pure ())
  for_ (zip [1 ..] opts) \(i, o) -> do
    let next :: ReverseQueue n => n ()
        next = chooseUpToM pid key source shown (n - 1) (drop i opts)
    case o of
      UpToLabel k act -> labeled k (act >> next)
      UpToTarget t act -> targeting t (act >> next)
