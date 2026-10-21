module LordOfTheRings.Location.Cards.TheHighlands (theHighlands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheHighlands = TheHighlands LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHighlands :: LocationCard TheHighlands
theHighlands = location TheHighlands Cards.theHighlands

-- | Travel: In order to travel to The Highlands, the players must reveal the top card of
-- the encounter deck, and add it to the staging area.
instance RunMessage TheHighlands where
  runMessage msg l@(TheHighlands a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      pure l
    _ -> TheHighlands <$> runMessage msg a
