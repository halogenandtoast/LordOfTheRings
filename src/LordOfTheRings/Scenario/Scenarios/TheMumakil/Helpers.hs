-- | What the cards of The Mûmakil share: the Wild Mûmakil, and the Capture objectives that
-- bring them down.
module LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (
  wildMumak,
  captureObjectives,
  capturedMumak,
  remainingHitPoints,
  printedCost,
  captureMumak,
  locationExploredThisRound,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardCost (StaticCost), HasCardDef, cdCost, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getHitPoints)
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.Types (ObjectiveAttrs)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (CaptureTrait))

wildMumak :: EnemyMatcher
wildMumak = EnemyWithCardCode (toCardCode Enemies.wildMumak)

-- | The Capture objectives in play.
captureObjectives :: HasGame m => m [ObjectiveAttrs]
captureObjectives = map toAttrs <$> objectivesWithTrait CaptureTrait

-- | The Wild Mûmak the Capture objective is attached to.
capturedMumak :: ObjectiveAttrs -> Maybe EnemyId
capturedMumak o = case o.placement of
  AttachedTo (EnemyTarget eid) -> Just eid
  _ -> Nothing

-- | An enemy's or a character's hit points less its damage.
remainingHitPoints :: HasGame m => Target -> m Int
remainingHitPoints target = do
  hp <- case target of
    EnemyTarget eid -> getEnemyHitPoints eid
    _ -> getHitPoints target
  subtract <$> getDamage target <*> pure hp

-- | A card's printed cost; an X or none counts as 0.
printedCost :: HasCardDef a => a -> Int
printedCost c = case cdCost (toCardDef c) of
  Just (StaticCost n) -> n
  _ -> 0

-- | "Add the objective and attached enemy to the victory display": the objective first, as
-- an enemy going there discards only attachments and treacheries.
captureMumak :: ReverseQueue m => ObjectiveAttrs -> EnemyId -> m ()
captureMumak o eid = do
  addToVictoryDisplay o
  addToVictoryDisplay eid

-- | Whether a location was explored this round, as Territorial Ape's shadow reads: the
-- scenario keeps it.
locationExploredThisRound :: HasGame m => m Bool
locationExploredThisRound = getsGame (fromMaybe False . parseMaybe parseJSON . (.meta) . toAttrs . (.scenario))
