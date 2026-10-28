module LordOfTheRings.Location.Cards.TheKingsLibrary (theKingsLibrary) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype TheKingsLibrary = TheKingsLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theKingsLibrary :: LocationCard TheKingsLibrary
theKingsLibrary = location TheKingsLibrary Cards.theKingsLibrary

-- | Forced: After the player who controls The King's Library lets an attack go undefended,
-- return The King's Library to the staging area.
instance HasAbilities TheKingsLibrary where
  getAbilities (TheKingsLibrary a) = returnsWhenUndefended a 1

-- | Travel: Reveal the top card of the encounter deck and add it to the staging area to
-- travel here.
instance RunMessage TheKingsLibrary where
  runMessage msg l@(TheKingsLibrary a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      TheKingsLibrary <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnToStaging a.id
      pure l
    _ -> TheKingsLibrary <$> runMessage msg a
