{- | What the cards of The Withered Heath share: the Dragon Signs in the victory display,
Cold-Drake, and the damage it keeps while set aside between stages 3 and 4.
-}
module LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (
  dragonSignsInVictoryDisplay,
  coldDrake,
  getColdDrake,
  setAsideDrakeDamage,
  setAsideColdDrake,
  healSetAsideColdDrake,
  returnColdDrake,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode), cardIs, cardMatches, getEnemy, select)
import LordOfTheRings.Message (Message (..))
import LordOfTheRings.Message.Lifted (ReverseQueue, addSetAsideToStaging, setAsideEnemy)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

-- | The X of Cold-Drake, Dragon Hatchling, Cave Entrance and stage 4B.
dragonSignsInVictoryDisplay :: HasGame m => m Int
dragonSignsInVictoryDisplay =
  getsGame (length . filter (cardMatches $ cardIs Objectives.dragonSign) . scenarioVictoryDisplay . toAttrs . gameScenario)

coldDrake :: EnemyMatcher
coldDrake = EnemyWithCardCode $ toCardCode Enemies.coldDrake

getColdDrake :: HasGame m => m (Maybe EnemyId)
getColdDrake = headMay <$> select coldDrake

{- | The damage set-aside Cold-Drake keeps (stage 3A: "Do not remove damage tokens from
it."), held in the scenario's meta from stage 3A until stage 4A brings it back.
-}
setAsideDrakeDamage :: HasGame m => m Int
setAsideDrakeDamage = getsGame (fromMaybe 0 . parseMaybe parseJSON . scenarioMeta . toAttrs . gameScenario)

{- | Stage 3A: "Set Cold-Drake aside, out of play. (Do not remove damage tokens from it.)" It
leaves play, and the scenario keeps its damage.
-}
setAsideColdDrake :: ReverseQueue m => m ()
setAsideColdDrake =
  getColdDrake >>= traverse_ \eid -> do
    damage <- (.damage) <$> getEnemy eid
    push $ ScenarioSpecific "witheredHeath.drakeSetAside" (toJSON damage)
    setAsideEnemy eid

-- | Stage 3B: "remove 3 damage tokens from the set-aside Cold-Drake."
healSetAsideColdDrake :: ReverseQueue m => Int -> m ()
healSetAsideColdDrake = push . ScenarioSpecific "witheredHeath.drakeHealed" . toJSON

{- | Stage 4A: "Add Cold-Drake to the staging area", with the damage it kept. The damage is
put back on it, not dealt: stage 4B's limit on the damage it takes each round does not
hold it back.
-}
returnColdDrake :: ReverseQueue m => m ()
returnColdDrake = do
  addSetAsideToStaging (cardIs Enemies.coldDrake)
  push . ScenarioSpecific "witheredHeath.drakeReturns" . toJSON =<< setAsideDrakeDamage
