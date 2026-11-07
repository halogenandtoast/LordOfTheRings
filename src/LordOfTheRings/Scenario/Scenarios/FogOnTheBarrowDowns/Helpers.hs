module LordOfTheRings.Scenario.Scenarios.FogOnTheBarrowDowns.Helpers (homeQuest, allFogQuests) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities, heldEntities)
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Quest.Types (Quest)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Prelude

allFogQuests :: HasGame m => m [Quest]
allFogQuests = getsGame $ \g -> concatMap (toList . entitiesQuests) (gameEntities g : heldEntities g)

homeQuest :: HasGame m => m (Maybe Quest)
homeQuest = find ((== toCardCode Quests.lostInTheFog) . toCardCode) <$> allFogQuests
