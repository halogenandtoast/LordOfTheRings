module LordOfTheRings.Location.Cards.GreatHall (greatHall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (placeProgressOnSmaugTheGolden)

newtype GreatHall = GreatHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Its burgle effect is read by burgle attempts only.
greatHall :: LocationCard GreatHall
greatHall = location GreatHall Cards.greatHall

-- | While Great Hall is the active location, it gains: "Forced: Put 1 progress on Smaug
-- the Golden at the end of each round."
instance HasAbilities GreatHall where
  getAbilities (GreatHall a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == ActiveLocation]

instance RunMessage GreatHall where
  runMessage msg l@(GreatHall a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgressOnSmaugTheGolden a 1
      pure l
    _ -> GreatHall <$> runMessage msg a
