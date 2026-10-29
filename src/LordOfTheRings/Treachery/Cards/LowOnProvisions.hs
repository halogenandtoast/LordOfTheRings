module LordOfTheRings.Treachery.Cards.LowOnProvisions (lowOnProvisions) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LowOnProvisions = LowOnProvisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lowOnProvisions :: TreacheryCard LowOnProvisions
lowOnProvisions = treachery LowOnProvisions Cards.lowOnProvisions

-- | When Revealed: Each player must assign X damage among characters he controls, where X is
-- the number of characters he controls.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character is
-- undamaged.)
instance RunMessage LowOnProvisions where
  runMessage msg t@(LowOnProvisions attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> dealDamageDivided attrs pid . length =<< charactersControlledBy pid
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      undamaged <- allM (fmap (== 0) . getDamage) defenders
      enemyGetsAttack attrs eid (if notNull defenders && undamaged then 2 else 1)
      pure t
    _ -> LowOnProvisions <$> runMessage msg attrs
