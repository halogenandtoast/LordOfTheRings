module LordOfTheRings.Enemy.Cards.GoblinRunners (goblinRunners) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinRunners = GoblinRunners EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinRunners :: EnemyCard GoblinRunners
goblinRunners = enemy GoblinRunners Cards.goblinRunners

-- | Surge.
-- Shadow: attacking enemy makes an additional attack immediately after this one. (Deal a
-- new shadow card for that attack.)
instance RunMessage GoblinRunners where
  runMessage msg e@(GoblinRunners attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> GoblinRunners <$> runMessage msg attrs
