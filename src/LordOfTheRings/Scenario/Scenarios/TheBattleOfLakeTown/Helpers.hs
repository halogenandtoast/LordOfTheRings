module LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies) where

import LordOfTheRings.Card (CardDef (cdName), toCardDef)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude

-- The rule sheet defines Smaug as any enemy with "Smaug" in its title.
smaugEnemies :: HasGame m => m [EnemyId]
smaugEnemies = filterM isSmaug =<< select AnyEnemy
 where
  isSmaug eid = do
    Name title _ <- cdName . toCardDef <$> getEnemy eid
    pure $ "Smaug" `isInfixOf` title
