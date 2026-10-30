-- | What the cards of Temple of the Deceived share: the Gate Key, and the Temple Guardian
-- the Temple locations call back.
module LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived.Helpers (
  getGateKey,
  gateKeyReady,
  templeGuardianReturns,
) where

import LordOfTheRings.Card (Card (EncounterCard), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Target

getGateKey :: HasGame m => m (Maybe ObjectiveAttrs)
getGateKey = getsGame (find ((== toCardCode Objectives.gateKey) . objectiveCardCode) . map toAttrs . toList . entitiesObjectives . gameEntities)

-- | The players control the Gate Key, ready: it is attached to a hero.
gateKeyReady :: HasGame m => m Bool
gateKeyReady = maybe False usable <$> getGateKey
 where
  usable k = not k.objectiveExhausted && case k.objectivePlacement of
    AttachedTo (HeroTarget _) -> True
    _ -> False

-- | Put Temple Guardian into play from the victory display, engaged with the player with
-- the highest threat.
templeGuardianReturns :: (ReverseQueue m, Targetable source) => source -> m ()
templeGuardianReturns source = do
  victory <- getsGame (scenarioVictoryDisplay . toAttrs . (.scenario))
  for_ [c | EncounterCard c <- victory, toCardCode c == toCardCode Enemies.templeGuardian] \card -> do
    removeFromVictoryDisplay (EncounterCard card)
    withHighestThreatPlayer source \pid -> putEnemyIntoPlayEngaged pid card
