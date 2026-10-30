module LordOfTheRings.Location.Cards.PatchOfWoods (patchOfWoods) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype PatchOfWoods = PatchOfWoods LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded. Safe. Immune to player card effects.
patchOfWoods :: LocationCard PatchOfWoods
patchOfWoods = location PatchOfWoods Cards.patchOfWoods

instance HasModifiersFor PatchOfWoods where
  getModifiersFor (PatchOfWoods a) = safeModifiers a

-- | Response: After you travel to Patch of Woods, each player adds 1 resource to each of his
-- heroes' resource pools.
instance HasAbilities PatchOfWoods where
  getAbilities (PatchOfWoods a) = mkAbility a 1 (response $ TraveledWindow After a.id) : safeAbilities a

instance RunMessage PatchOfWoods where
  runMessage msg l@(PatchOfWoods a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      for_ heroes \h -> placeResources a h 1
      pure l
    _ -> do
      runSafe msg a
      PatchOfWoods <$> runMessage msg a
