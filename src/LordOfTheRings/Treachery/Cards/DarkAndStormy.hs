module LordOfTheRings.Treachery.Cards.DarkAndStormy (darkAndStormy) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarkAndStormy = DarkAndStormy TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkAndStormy :: TreacheryCard DarkAndStormy
darkAndStormy = treachery DarkAndStormy Cards.darkAndStormy

-- | Surge.
-- When Revealed: Deal 1 shadow card to each Ship enemy in play. Until the end of the
-- round, shadow effects cannot be canceled.
-- Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage DarkAndStormy where
  runMessage msg t@(DarkAndStormy attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      ships <- select shipEnemy
      for_ ships dealShadowCard
      roundModifier attrs GameTarget CannotCancelShadowEffects
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> DarkAndStormy <$> runMessage msg attrs
