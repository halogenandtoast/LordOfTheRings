module LordOfTheRings.Location.Cards.SouthwardRoad (southwardRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (cardsUnderTheCrossRoads, revealFromUnderTheCrossRoads)

newtype SouthwardRoad = SouthwardRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southwardRoad :: LocationCard SouthwardRoad
southwardRoad = location SouthwardRoad Cards.southwardRoad

-- | While Southward Road is the active location, each player may optionally engage an
-- additional enemy during the encounter phase.
-- Travel: The first player reveals the top facedown card underneath The Cross-roads and
-- adds it to the staging area to travel here: not without one to reveal.
instance HasModifiersFor SouthwardRoad where
  getModifiersFor (SouthwardRoad a) = do
    when (a.placement == ActiveLocation) do
      players <- getRemainingPlayers
      for_ players \p -> modified a (PlayerTarget p) [OptionallyEngagesAdditionalEnemy]
    under <- cardsUnderTheCrossRoads
    when (null under) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage SouthwardRoad where
  runMessage msg (SouthwardRoad a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealFromUnderTheCrossRoads Nothing
      SouthwardRoad <$> runMessage msg a
    _ -> SouthwardRoad <$> runMessage msg a
