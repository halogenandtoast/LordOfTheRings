module LordOfTheRings.Location.Cards.BuckleburyFerry (buckleburyFerry) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (winAShadowOfThePast)

newtype BuckleburyFerry = BuckleburyFerry LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

buckleburyFerry :: LocationCard BuckleburyFerry
buckleburyFerry = location BuckleburyFerry Cards.buckleburyFerry

-- | Immune to player card effects. While there is at least 1 other location in play,
-- players cannot travel here.
instance HasModifiersFor BuckleburyFerry where
  getModifiersFor (BuckleburyFerry a) = do
    others <- selectAny (NotLocation $ LocationWithId a.id)
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | others]

{- | Travel: The first player must make a Hide 3 test to travel here. Making it is the cost:
the players travel whether it passes or fails (The Black Riders rules insert's example
has the test made "to travel to Bucklebury Ferry").

After Bucklebury Ferry leaves play as an explored location, the players win the game.
-}
instance RunMessage BuckleburyFerry where
  runMessage msg (BuckleburyFerry a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      hideTest lead a 3
      BuckleburyFerry <$> runMessage msg a
    CheckWindows ws | Window When (Explored a.id) `elem` ws -> do
      winAShadowOfThePast
      BuckleburyFerry <$> runMessage msg a
    _ -> BuckleburyFerry <$> runMessage msg a
