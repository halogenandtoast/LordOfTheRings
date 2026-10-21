module LordOfTheRings.Event.Cards.DaeronsRunes (daeronsRunes) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype DaeronsRunes = DaeronsRunes EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Draw 2 cards. Then, discard 1 card from your hand.
daeronsRunes :: EventCard DaeronsRunes
daeronsRunes = event DaeronsRunes Cards.daeronsRunes

instance RunMessage DaeronsRunes where
  runMessage msg e@(DaeronsRunes attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      drawCards pid 2
      chooseAndDiscardFromHand pid attrs 1
      pure e
    _ -> DaeronsRunes <$> runMessage msg attrs
