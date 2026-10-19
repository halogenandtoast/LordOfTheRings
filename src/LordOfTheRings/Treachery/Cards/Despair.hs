module LordOfTheRings.Treachery.Cards.Despair (despair) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Despair = Despair TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

despair :: TreacheryCard Despair
despair = treachery Despair Cards.despair

-- | When Revealed: Remove 4 progress tokens from the current quest card.
-- Shadow: Defending character does not count its defense.
instance RunMessage Despair where
  runMessage msg t@(Despair attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      removeProgressFromQuest attrs 4
      pure t
    ResolveShadowEffect cid _ _ defender | cid == attrs.cardId -> do
      for_ defender \d -> phaseModifier attrs d DoesNotCountDefense
      pure t
    _ -> Despair <$> runMessage msg attrs
