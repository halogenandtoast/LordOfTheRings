-- | What the cards of The Fate of Wilderland share: Urdug, and the two objectives that
-- keep the score of the battle.
module LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (
  getUrdug,
  getGoblinsAssault,
  getHeroesDefense,
  goblinsAssaultProgress,
  placeAssaultProgress,
  assaultProgressLabeled,
  gateInStaging,
) where

import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types ()
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target (Target (ObjectiveTarget))

getUrdug :: HasGame m => m (Maybe EnemyId)
getUrdug = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.urdug)

objectiveIn :: HasGame m => CardDef -> m (Maybe ObjectiveId)
objectiveIn def =
  getsGame (fmap toId . find ((== toCardCode def) . toCardCode) . toList . entitiesObjectives . gameEntities)

getGoblinsAssault :: HasGame m => m (Maybe ObjectiveId)
getGoblinsAssault = objectiveIn Objectives.theGoblinsAssault

getHeroesDefense :: HasGame m => m (Maybe ObjectiveId)
getHeroesDefense = objectiveIn Objectives.theHeroesDefense

-- | The progress on The Goblins' Assault.
goblinsAssaultProgress :: HasGame m => m Int
goblinsAssaultProgress =
  getGoblinsAssault >>= \case
    Nothing -> pure 0
    Just oid -> (.progress) <$> getObjective oid

-- | "Place X progress on The Goblins' Assault."
placeAssaultProgress :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeAssaultProgress source n = when (n > 0) $ getGoblinsAssault >>= traverse_ \oid -> placeProgress source (ObjectiveTarget oid) n

-- | The "or place 1 progress on The Goblins' Assault" choice the cards of the scenario offer.
assaultProgressLabeled :: (ReverseQueue m, Sourceable source) => source -> ChooseT m ()
assaultProgressLabeled source = labeled "theFateOfWilderland.placeProgress" $ placeAssaultProgress source 1

-- | Gate of Gundabad in the staging area: "While Gate of Gundabad is in the staging area,
-- resources cannot be placed on The Heroes' Defense."
gateInStaging :: LocationMatcher
gateInStaging = LocationWithCardCode (toCardCode Locations.gateOfGundabad) <> LocationInStagingArea
