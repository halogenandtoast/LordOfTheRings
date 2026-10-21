module LordOfTheRings.Location.Cards.FensAndMires (fensAndMires) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)

newtype FensAndMires = FensAndMires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fensAndMires :: LocationCard FensAndMires
fensAndMires = location FensAndMires Cards.fensAndMires

-- | Forced: After the players travel to this location, place 1 resource token on Gollum.
instance HasAbilities FensAndMires where
  getAbilities (FensAndMires a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage FensAndMires where
  runMessage msg l@(FensAndMires a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeOnGollum a 1
      pure l
    _ -> FensAndMires <$> runMessage msg a
