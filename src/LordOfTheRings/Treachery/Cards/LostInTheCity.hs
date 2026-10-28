module LordOfTheRings.Treachery.Cards.LostInTheCity (lostInTheCity) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Trait (Trait (City))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LostInTheCity = LostInTheCity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheCity :: TreacheryCard LostInTheCity
lostInTheCity = treachery LostInTheCity Cards.lostInTheCity

-- | While it is being revealed, its "when revealed" effect cannot be canceled.
instance HasModifiersFor LostInTheCity where
  getModifiersFor (LostInTheCity a) = when (a.placement == Unplaced) $ modified a GameTarget [CannotCancelEncounterEffects]

-- | When Revealed: Each player must search the encounter deck and discard pile for 1 City
-- location and add it to the staging area, if able. Shuffle the encounter deck. This
-- effect cannot be canceled.
-- Shadow: Defending player discards his hand.
instance RunMessage LostInTheCity where
  runMessage msg t@(LostInTheCity attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckAndDiscard p attrs (CardWithTrait City <> CardWithType LocationType)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardHand pid
      pure t
    _ -> LostInTheCity <$> runMessage msg attrs
