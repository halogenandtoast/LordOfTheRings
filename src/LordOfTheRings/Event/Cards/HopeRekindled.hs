module LordOfTheRings.Event.Cards.HopeRekindled (hopeRekindled) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype HopeRekindled = HopeRekindled EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Reduce the cost of the next event that has a Valour trigger you play this
-- phase by 2. Valour Action: Search the top 10 cards of your deck for an event that has a
-- Valour trigger and add it to your hand. Shuffle your deck.
hopeRekindled :: EventCard HopeRekindled
hopeRekindled = event HopeRekindled Cards.hopeRekindled

instance RunMessage HopeRekindled where
  runMessage msg e@(HopeRekindled attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      let valourEvent = CardWithType EventType <> CardWithValour
      chooseOrRunOneM pid do
        labeled "hopeRekindled.reduce" $ phaseModifier attrs (PlayerTarget pid) (LowerNextCostOf valourEvent 2)
        valourLabeled pid $ searchDeck pid attrs 10 valourEvent False
      pure e
    _ -> HopeRekindled <$> runMessage msg attrs
