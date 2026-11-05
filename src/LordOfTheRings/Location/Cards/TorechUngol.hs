module LordOfTheRings.Location.Cards.TorechUngol (torechUngol) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (getShelob)
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (smeagolTravelModifiers, exhaustSmeagolToTravel)

newtype TorechUngol = TorechUngol LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

torechUngol :: LocationCard TorechUngol
torechUngol = location TorechUngol Cards.torechUngol

instance HasModifiersFor TorechUngol where
  getModifiersFor (TorechUngol a) = do
    smeagolTravelModifiers a
    when (a.placement == StagingArea) do
      spider <- getShelob
      for_ spider \eid -> modified a eid [AttackModifier 1, DefenseModifier 1]

instance RunMessage TorechUngol where
  runMessage msg (TorechUngol a) = case msg of
    TravelTo lid | lid == a.id -> do
      exhaustSmeagolToTravel
      TorechUngol <$> runMessage msg a
    _ -> TorechUngol <$> runMessage msg a
