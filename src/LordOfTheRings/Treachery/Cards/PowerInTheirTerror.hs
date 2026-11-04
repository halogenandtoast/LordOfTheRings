module LordOfTheRings.Treachery.Cards.PowerInTheirTerror (powerInTheirTerror) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PowerInTheirTerror = PowerInTheirTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

powerInTheirTerror :: TreacheryCard PowerInTheirTerror
powerInTheirTerror = treachery PowerInTheirTerror Cards.powerInTheirTerror

-- | When Revealed: Each ally gets -1 willpower until the end of the round. Deal 2 damage to
-- each ally with 0 willpower, read once the -1 applies.
-- Shadow: Discard a non-objective attachment you control.
instance RunMessage PowerInTheirTerror where
  runMessage msg t@(PowerInTheirTerror attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- select AnyAlly
      for_ allies \a -> roundModifier attrs a (WillpowerModifier (-1))
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      allies <- select AnyAlly
      for_ allies \a -> do
        willpower <- getWillpower (toTarget a)
        when (willpower <= 0) $ dealDamage attrs a 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure t
    _ -> PowerInTheirTerror <$> runMessage msg attrs
