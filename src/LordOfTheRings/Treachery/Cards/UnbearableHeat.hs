module LordOfTheRings.Treachery.Cards.UnbearableHeat (unbearableHeat) where

import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (temperatureX)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnbearableHeat = UnbearableHeat TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

unbearableHeat :: TreacheryCard UnbearableHeat
unbearableHeat = treachery UnbearableHeat Cards.unbearableHeat

-- | When Revealed: Each player assigns X damage among characters he controls, where X is
-- the tens digit of the temperature.
-- Shadow: Attacking enemy gets +X attack, where X is the tens digit of the temperature.
instance RunMessage UnbearableHeat where
  runMessage msg t@(UnbearableHeat attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- temperatureX
      when (x > 0) $ eachPlayer \p -> dealDamageDivided attrs p x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- temperatureX
      enemyGetsAttack attrs eid x
      pure t
    _ -> UnbearableHeat <$> runMessage msg attrs
