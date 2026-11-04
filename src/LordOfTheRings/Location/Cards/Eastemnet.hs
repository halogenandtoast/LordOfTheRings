module LordOfTheRings.Location.Cards.Eastemnet (eastemnet) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype Eastemnet = Eastemnet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eastemnet :: LocationCard Eastemnet
eastemnet = location Eastemnet Cards.eastemnet

-- | While Eastemnet is the active location, it gains: "Response: After Eastemnet is
-- explored, add 1 resource to each hero's resource pool."
instance HasAbilities Eastemnet where
  getAbilities (Eastemnet a) = [mkAbility a 1 $ response $ ExploredWindow After a.id | a.placement == ActiveLocation]

-- | Travel: Increase the pursuit value by 1.
instance RunMessage Eastemnet where
  runMessage msg l@(Eastemnet a) = case msg of
    TravelTo lid | lid == a.id -> do
      increasePursuit 1
      Eastemnet <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      for_ heroes \h -> placeResources a h 1
      pure l
    _ -> Eastemnet <$> runMessage msg a
