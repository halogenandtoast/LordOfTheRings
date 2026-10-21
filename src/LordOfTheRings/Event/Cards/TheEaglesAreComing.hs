module LordOfTheRings.Event.Cards.TheEaglesAreComing (theEaglesAreComing) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Eagle))

newtype TheEaglesAreComing = TheEaglesAreComing EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Search the top 5 cards of your deck for any number of Eagle cards and add them
-- to your hand. Shuffle the other cards back into your deck.
theEaglesAreComing :: EventCard TheEaglesAreComing
theEaglesAreComing = event TheEaglesAreComing Cards.theEaglesAreComing

instance RunMessage TheEaglesAreComing where
  runMessage msg e@(TheEaglesAreComing attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      searchDeck pid attrs 5 (CardWithTrait Eagle) True
      pure e
    _ -> TheEaglesAreComing <$> runMessage msg attrs
