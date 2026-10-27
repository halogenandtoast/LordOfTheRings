module LordOfTheRings.Event.Cards.TheEndComes (theEndComes) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype TheEndComes = TheEndComes EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a Dwarf character leaves play, shuffle the encounter discard pile
-- back into the encounter deck.
theEndComes :: EventCard TheEndComes
theEndComes = eventWith TheEndComes Cards.theEndComes (windowL ?~ CharacterLeftPlayWindow After (CardWithTrait Dwarf))

instance RunMessage TheEndComes where
  runMessage msg e@(TheEndComes attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      pure e
    _ -> TheEndComes <$> runMessage msg attrs
