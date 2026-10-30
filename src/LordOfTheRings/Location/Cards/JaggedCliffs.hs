module LordOfTheRings.Location.Cards.JaggedCliffs (jaggedCliffs) where

import LordOfTheRings.Helpers.IslandMap (adjacentLocations, locationToThe)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Grid (Direction (RightOf))
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Uncharted, under a Lost Island side.
newtype JaggedCliffs = JaggedCliffs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

jaggedCliffs :: LocationCard JaggedCliffs
jaggedCliffs = location JaggedCliffs Cards.jaggedCliffs

-- | While Jagged Cliffs is the active location, the location to its right is not
-- considered to be in the staging area.
instance HasModifiersFor JaggedCliffs where
  getModifiersFor (JaggedCliffs a) = when (a.placement == ActiveLocation) do
    right <- locationToThe RightOf a
    for_ right \r -> modified a r [NotInStagingArea]

-- | Forced: When Jagged Cliffs is flipped over while active, look at the facedown side of
-- each adjacent copy of Lost Island.
instance HasAbilities JaggedCliffs where
  getAbilities (JaggedCliffs a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

-- | Each player looks, as the players share what they see.
instance RunMessage JaggedCliffs where
  runMessage msg l@(JaggedCliffs a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      islands <- filter isUncharted <$> adjacentLocations a
      eachPlayer \p -> for_ islands \i -> lookAtFacedownSide p i.id
      pure l
    _ -> JaggedCliffs <$> runMessage msg a
