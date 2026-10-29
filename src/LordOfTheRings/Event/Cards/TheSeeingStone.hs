module LordOfTheRings.Event.Cards.TheSeeingStone (theSeeingStone) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheSeeingStone = TheSeeingStone EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 1, on its card def. Action: Search your deck for a card with the Doomed keyword
-- and add it to your hand. Shuffle your deck.
theSeeingStone :: EventCard TheSeeingStone
theSeeingStone = event TheSeeingStone Cards.theSeeingStone

instance RunMessage TheSeeingStone where
  runMessage msg e@(TheSeeingStone attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      deck <- (.deck) <$> getPlayer pid
      searchDeck pid attrs (length deck) CardWithDoomed False
      pure e
    _ -> TheSeeingStone <$> runMessage msg attrs
