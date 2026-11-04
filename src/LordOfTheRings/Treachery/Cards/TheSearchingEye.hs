module LordOfTheRings.Treachery.Cards.TheSearchingEye (theSearchingEye) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheSearchingEye = TheSearchingEye TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden with a player card back, shuffled into a player's deck. The Searching Eye
functions like a player card: drawn, it stays in its player's hand, and discarded it goes
to his discard pile (see 'FunctionsLikeAPlayerCard').
-}
theSearchingEye :: TreacheryCard TheSearchingEye
theSearchingEye = treachery TheSearchingEye Cards.theSearchingEye

-- | Forced: After you draw this card, either exhaust each hero you control, or reveal the
-- top card of the encounter deck. (Its card, out of play in the hand, answers the draw.)
instance RunMessage TheSearchingEye where
  runMessage msg t@(TheSearchingEye attrs) = case msg of
    EncounterCardDrawn pid card | card.id == attrs.cardId -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid))
      chooseOneM pid do
        questionLabeled "theSearchingEye.choose"
        showing [EncounterCard card]
        labeled "theSearchingEye.exhaust" $ traverse_ exhaust heroes
        labeled "theSearchingEye.reveal" $ revealEncounterCardAs pid
      pure t
    _ -> TheSearchingEye <$> runMessage msg attrs
