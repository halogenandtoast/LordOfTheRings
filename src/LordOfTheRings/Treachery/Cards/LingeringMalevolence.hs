module LordOfTheRings.Treachery.Cards.LingeringMalevolence (lingeringMalevolence) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LingeringMalevolence = LingeringMalevolence TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lingeringMalevolence :: TreacheryCard LingeringMalevolence
lingeringMalevolence = treachery LingeringMalevolence Cards.lingeringMalevolence

-- | When Revealed: The first player discards the bottom card of his deck. Then, he discards
-- each card in his hand with a printed cost higher than that of the discarded card.
-- Shadow: Discard the bottom card of the defending player's deck. If that card's printed
-- cost is 2 or less, attacking enemy gets +2 attack.
instance RunMessage LingeringMalevolence where
  runMessage msg t@(LingeringMalevolence attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      hand <- (.hand) <$> getPlayer lead
      bottom <- discardBottomOfDeck lead
      for_ bottom \b -> for_ [c | c <- hand, printedCostOf c > printedCostOf b] (discardFromHand lead)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      bottom <- discardBottomOfDeck pid
      when (maybe False ((<= 2) . printedCostOf) bottom) $ enemyGetsAttack (CardIdSource cid) eid 2
      pure t
    _ -> LingeringMalevolence <$> runMessage msg attrs
