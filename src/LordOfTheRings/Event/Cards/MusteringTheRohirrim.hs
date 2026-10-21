module LordOfTheRings.Event.Cards.MusteringTheRohirrim (musteringTheRohirrim) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype MusteringTheRohirrim = MusteringTheRohirrim EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Search the top 10 cards of your deck for any 1 Rohan ally card and add it to
-- your hand. Then, shuffle the other cards back into your deck.
musteringTheRohirrim :: EventCard MusteringTheRohirrim
musteringTheRohirrim = event MusteringTheRohirrim Cards.musteringTheRohirrim

instance RunMessage MusteringTheRohirrim where
  runMessage msg e@(MusteringTheRohirrim attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      searchDeck pid attrs 10 (CardWithTrait Rohan <> CardWithType AllyType) False
      pure e
    _ -> MusteringTheRohirrim <$> runMessage msg attrs
