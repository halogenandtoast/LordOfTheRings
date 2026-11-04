module LordOfTheRings.Location.Cards.GreatCorsairShip (greatCorsairShip) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (corsairFleetResources, searchForCorsairsWithACaptain, shipLocation)

newtype GreatCorsairShip = GreatCorsairShip LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatCorsairShip :: LocationCard GreatCorsairShip
greatCorsairShip = location GreatCorsairShip Cards.greatCorsairShip

-- | X is the number of resources on The Corsair Fleet. The players cannot travel here
-- unless there are at least 3 Ship locations in the victory display.
instance HasModifiersFor GreatCorsairShip where
  getModifiersFor (GreatCorsairShip a) = do
    x <- corsairFleetResources
    ships <- getsGame (length . filter (cardMatches shipLocation) . (.victoryDisplay) . toAttrs . gameScenario)
    modifySelf a $ [ThreatModifier x | x > 0] <> [CannotBeTraveledTo | ships < 3]

-- | Travel: Each player searches the encounter deck and discard pile for a Corsair enemy
-- and adds it to the staging area. One of those enemies must be Corsair Captain, if able.
instance RunMessage GreatCorsairShip where
  runMessage msg (GreatCorsairShip a) = case msg of
    TravelTo lid | lid == a.id -> do
      searchForCorsairsWithACaptain a
      GreatCorsairShip <$> runMessage msg a
    _ -> GreatCorsairShip <$> runMessage msg a
