-- | What The Long Dark's cards share: locate tests and "Lost:" effects.
module LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (
  locateTest,
  passesLocateTest,
  triggerLostEffects,
) where

import LordOfTheRings.Card (EncounterCard, toCardCode)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Source

{- | The first player makes a locate test: the top card of the encounter deck is
discarded, and what follows a fail and a pass are actions of their own. The scenario
resolves it.
-}
locateTest
  :: (ReverseQueue m, Sourceable source) => source -> QueueT Message m () -> QueueT Message m () -> m ()
locateTest source onFail onPass = do
  failed <- capture onFail
  passed <- capture onPass
  push $ LocateTest (toSource source) failed passed

-- | Trigger all "Lost:" effects in play.
triggerLostEffects :: (ReverseQueue m, Sourceable source) => source -> m ()
triggerLostEffects = push . TriggerLostEffects . toSource

-- | The cards that carry the locate test's pass icon (Hall of Beorn marks them PASS).
passesLocateTest :: EncounterCard -> Bool
passesLocateTest = (`elem` passing) . toCardCode

passing :: [CardCode]
passing = ["04089", "04090", "04091", "04092", "04094", "04095"]
