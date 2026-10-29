module LordOfTheRings.Enemy.Cards.DunlandTribesman (dunlandTribesman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DunlandTribesman = DunlandTribesman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlandTribesman :: EnemyCard DunlandTribesman
dunlandTribesman = enemy DunlandTribesman Cards.dunlandTribesman

-- | Forced: After a player draws any number of cards, Dunland Tribesman gets +1 threat until
-- the end of the round.
instance HasAbilities DunlandTribesman where
  getAbilities (DunlandTribesman a) = [mkAbility a 1 $ forced $ DrewCardsWindow After Nothing]

-- | When Revealed: Each player draws a card.
-- Shadow: Attacking enemy gets +1 attack.
instance RunMessage DunlandTribesman where
  runMessage msg e@(DunlandTribesman attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> drawCards attrs pid 1
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifier attrs attrs (ThreatModifier 1)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      pure e
    _ -> DunlandTribesman <$> runMessage msg attrs
