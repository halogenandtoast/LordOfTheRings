-- | What The Seventh Level's nightmare cards share: resources on Goblin enemies.
module LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (
  resourcesOn,
  mostGoblinResources,
  addResourceToEachGoblin,
  addResourceIfGoblin,
  firstInPlay,
) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Token (Token (Resource), countTokens)
import LordOfTheRings.Trait (Trait (Goblin))

-- | The resources on the enemy.
resourcesOn :: HasGame m => EnemyId -> m Int
resourcesOn eid = countTokens Resource . enemyTokens . toAttrs <$> getEnemy eid

-- | The highest number of resources on a Goblin enemy in play.
mostGoblinResources :: HasGame m => m Int
mostGoblinResources = foldr max 0 <$> (traverse resourcesOn =<< select (EnemyWithTrait Goblin))

-- | "Add 1 resource to each Goblin enemy in play."
addResourceToEachGoblin :: (ReverseQueue m, Sourceable source) => source -> m ()
addResourceToEachGoblin source = do
  goblins <- select (EnemyWithTrait Goblin)
  for_ goblins \g -> placeResources source g 1

-- | "If the attacking enemy is a Goblin, add 1 resource to it."
addResourceIfGoblin :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> m ()
addResourceIfGoblin source eid = whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Goblin) $ placeResources source eid 1

{- | Whether the enemy is the first copy of its card in play: of several, it stands for
them all, as an ability that does not stack with other copies.
-}
firstInPlay :: HasGame m => EnemyId -> m Bool
firstInPlay eid = do
  code <- toCardCode <$> getEnemy eid
  (== Just eid) . minimumMay <$> select (EnemyWithCardCode code)
