module LordOfTheRings.Event.Cards.ElvenLight (elvenLight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ElvenLight = ElvenLight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Elven-light can only be played from your discard pile, as its card def says.
elvenLight :: EventCard ElvenLight
elvenLight = event ElvenLight Cards.elvenLight

-- | Action: Return Elven-light to your hand from your discard pile. Then, draw 1 card.
-- Played, it is back in the discard pile by the time it returns.
instance RunMessage ElvenLight where
  runMessage msg e@(ElvenLight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      returnFromDiscardToHand pid card.id
      drawCards pid 1
      pure e
    _ -> ElvenLight <$> runMessage msg attrs
