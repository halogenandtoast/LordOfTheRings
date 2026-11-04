module LordOfTheRings.Treachery.Cards.OvercomeByTerror (overcomeByTerror) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing, theOneRingExhausted)
import LordOfTheRings.Treachery.Import.Lifted

newtype OvercomeByTerror = OvercomeByTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Flight to the Ford burden set. Surge.
overcomeByTerror :: TreacheryCard OvercomeByTerror
overcomeByTerror = treachery OvercomeByTerror Cards.overcomeByTerror

-- | When Revealed: The first player must exhaust The One Ring. If The One Ring is already
-- exhausted, the first player must discard 2 cards at random from his hand.
-- Shadow: Defending player discards 1 card at random from his hand.
instance RunMessage OvercomeByTerror where
  runMessage msg t@(OvercomeByTerror attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      exhausted <- theOneRingExhausted
      if exhausted
        then discardRandomFromHand lead 2
        else traverse_ (exhaust . ObjectiveTarget . (.id)) =<< getSagaOneRing
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardRandomFromHand pid 1
      pure t
    _ -> OvercomeByTerror <$> runMessage msg attrs
