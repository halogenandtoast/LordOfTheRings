module LordOfTheRings.Enemy.Cards.ThingsInThePools (thingsInThePools) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

newtype ThingsInThePools = ThingsInThePools EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thingsInThePools :: EnemyCard ThingsInThePools
thingsInThePools = enemy ThingsInThePools Cards.thingsInThePools

-- | Forced: After a character commits to an escape test, deal 1 damage to that character.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if attacking enemy is Undead).
instance RunMessage ThingsInThePools where
  runMessage msg e@(ThingsInThePools attrs) = case msg of
    CheckWindows ws | committed@(_ : _) <- [c | Window After (CommittedToEscapeTest c) <- ws] -> do
      for_ committed \c -> dealDamage attrs c 1
      ThingsInThePools <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      undead <- selectAny (EnemyWithId eid <> EnemyWithTrait Undead)
      enemyGetsAttack attrs eid (if undead then 2 else 1)
      pure e
    _ -> ThingsInThePools <$> runMessage msg attrs
