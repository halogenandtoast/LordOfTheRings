module LordOfTheRings.Location.Cards.OutlyingHomestead (outlyingHomestead) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OutlyingHomestead = OutlyingHomestead LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

outlyingHomestead :: LocationCard OutlyingHomestead
outlyingHomestead = location OutlyingHomestead Cards.outlyingHomestead

-- | Doomed 1. While Outlying Homestead is in the staging area, players cannot reduce their
-- threat.
instance HasModifiersFor OutlyingHomestead where
  getModifiersFor (OutlyingHomestead a) =
    when (a.placement == StagingArea) do
      players <- getRemainingPlayers
      for_ players \p -> modified a (PlayerTarget p) [CannotReduceThreat]

-- | Travel: Reveal a card from the encounter deck.
instance RunMessage OutlyingHomestead where
  runMessage msg l@(OutlyingHomestead a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      pure l
    _ -> OutlyingHomestead <$> runMessage msg a
