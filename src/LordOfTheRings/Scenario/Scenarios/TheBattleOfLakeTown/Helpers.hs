module LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies, lakeTownObjectives, lakeTownDamage) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Target (Target (ObjectiveTarget))
import LordOfTheRings.Prelude

-- The rule sheet defines Smaug as any enemy with "Smaug" in its title.
smaugEnemies :: HasGame m => m [EnemyId]
smaugEnemies = select (EnemyWithTitleContaining "Smaug")

lakeTownObjectives :: HasGame m => m [ObjectiveId]
lakeTownObjectives = getsGame $ map toId . filter ((== toCardCode Objectives.lakeTown) . toCardCode) . toList . (.entities.objectives)

lakeTownDamage :: HasGame m => m Int
lakeTownDamage = sum <$> (traverse (getDamage . ObjectiveTarget) =<< lakeTownObjectives)
