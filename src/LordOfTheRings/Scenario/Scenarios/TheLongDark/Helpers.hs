-- | What The Long Dark's cards share: locate tests and "Lost:" effects.
module LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (
  locateTest,
  passesLocateTest,
  triggerLostEffects,
  lostTriggered,
  lostEffectsInPlay,
  chooseLostEffects,
) where

import LordOfTheRings.Card (EncounterCard, toCardCode)
import LordOfTheRings.Card.Code (CardCode, HasCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities ()
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (inNightmareMode)
import LordOfTheRings.Target
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Source

{- | The first player makes a locate test: he discards cards from his hand, each to
discard the top card of the encounter deck, until one carries the pass icon or he stops.
What follows a fail and a pass are actions of their own. The scenario resolves it.
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

-- | Whether the message triggers the card's "Lost:" effect: all of them do, or its own
-- alone.
lostTriggered :: Targetable a => a -> Message -> Bool
lostTriggered a = \case
  TriggerLostEffects _ -> True
  TriggerLostEffectOf _ t -> isTarget a t
  _ -> False

-- | The cards in play with a "Lost:" effect, and the scenario for its nightmare setup
-- card's.
lostEffectsInPlay :: HasGame m => m [Target]
lostEffectsInPlay = do
  es <- getsGame gameEntities
  nightmare <- inNightmareMode
  let lost :: (HasCardCode a, Targetable a) => [a] -> [Target]
      lost = map toTarget . filter ((`elem` lostCards) . toCardCode)
  pure $ lost (toList es.enemies) <> lost (toList es.locations) <> lost (toList es.treacheries) <> [ScenarioTarget | nightmare]
 where
  lostCards =
    map
      toCardCode
      [ Enemies.goblinWarlord
      , Enemies.trapdoorSpider
      , Locations.abandonedMine
      , Locations.dwarvenForge
      , Locations.silentCaverns
      , Treacheries.lostAndHelpless
      ]

-- | Each of the players, in turn, chooses and triggers a different "Lost:" effect in play.
chooseLostEffects :: (ReverseQueue m, Sourceable source) => source -> [PlayerId] -> m ()
chooseLostEffects source pids = push $ ChooseLostEffects (toSource source) pids []
