module LordOfTheRings.Enemy.Cards.BalefulShade (balefulShade) where

import LordOfTheRings.Card.CardType (CardType (AllyType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (discardTopCards)

newtype BalefulShade = BalefulShade EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

balefulShade :: EnemyCard BalefulShade
balefulShade = enemy BalefulShade Cards.balefulShade

-- | Forced: When Baleful Shade attacks, the defending player discards the top card of his
-- deck. If the discarded card is an ally, Baleful Shade gets +2 attack for this attack.
instance HasAbilities BalefulShade where
  getAbilities (BalefulShade a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Exhaust a character you control.
instance RunMessage BalefulShade where
  runMessage msg e@(BalefulShade attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- windows] \pid -> do
        top <- discardTopCards pid 1
        when (any ((== AllyType) . (.cardType)) top) $ enemyGetsAttack attrs attrs.id 2
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure e
    _ -> BalefulShade <$> runMessage msg attrs
