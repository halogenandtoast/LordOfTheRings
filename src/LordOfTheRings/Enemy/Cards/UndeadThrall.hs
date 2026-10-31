module LordOfTheRings.Enemy.Cards.UndeadThrall (undeadThrall) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UndeadThrall = UndeadThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Indestructible. Surge.
undeadThrall :: EnemyCard UndeadThrall
undeadThrall = enemy UndeadThrall Cards.undeadThrall

-- | Shadow: Put Undead Thrall into play engaged with the defending player and deal it a
-- shadow card. In play, it deals itself the shadow card.
instance RunMessage UndeadThrall where
  runMessage msg e@(UndeadThrall attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      putShadowEnemyIntoPlay eid cid pid
      doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      dealShadowCard attrs.id
      pure e
    _ -> UndeadThrall <$> runMessage msg attrs
