module LordOfTheRings.Treachery.Cards.WhatsInMyPocket (whatsInMyPocket) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (answerRiddle)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WhatsInMyPocket = WhatsInMyPocket TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

whatsInMyPocket :: TreacheryCard WhatsInMyPocket
whatsInMyPocket = treachery WhatsInMyPocket Cards.whatsInMyPocket

-- | This effect cannot be canceled.
instance HasModifiersFor WhatsInMyPocket where
  getModifiersFor (WhatsInMyPocket a) = modifySelf a [WhenRevealedCannotBeCanceled]

-- | When Revealed: The first player must choose to answer the riddle on this card.
instance RunMessage WhatsInMyPocket where
  runMessage msg t@(WhatsInMyPocket attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      answerRiddle attrs False (MkEncounterCard attrs.cardId (toCardCode attrs))
      pure t
    _ -> WhatsInMyPocket <$> runMessage msg attrs
