module LordOfTheRings.Enemy.Cards.GoblinSkirmisher (goblinSkirmisher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (firstInPlay, resourcesOn)
import LordOfTheRings.Trait (Trait (Goblin))

newtype GoblinSkirmisher = GoblinSkirmisher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinSkirmisher :: EnemyCard GoblinSkirmisher
goblinSkirmisher = enemy GoblinSkirmisher Cards.goblinSkirmisher

-- | Each Goblin enemy gets +1 attack for each resource on it. This ability does not stack
-- with other copies of Goblin Skirmisher: the first copy in play gives it.
instance HasModifiersFor GoblinSkirmisher where
  getModifiersFor (GoblinSkirmisher a) = whenM (firstInPlay a.id) do
    goblins <- select (EnemyWithTrait Goblin)
    for_ goblins \g -> do
      n <- resourcesOn g
      when (n > 0) $ modified a g [AttackModifier n]

-- | Shadow: Deal X damage to the defending character. X is the number of resources on the
-- attacking enemy.
instance RunMessage GoblinSkirmisher where
  runMessage msg e@(GoblinSkirmisher attrs) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      x <- resourcesOn eid
      when (x > 0) $ for_ defenders \d -> dealDamage (EnemySource eid) d x
      pure e
    _ -> GoblinSkirmisher <$> runMessage msg attrs
