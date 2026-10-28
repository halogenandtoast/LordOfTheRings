module LordOfTheRings.Location.Cards.GardenOfPoisons (gardenOfPoisons) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GardenOfPoisons = GardenOfPoisons LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gardenOfPoisons :: LocationCard GardenOfPoisons
gardenOfPoisons = location GardenOfPoisons Cards.gardenOfPoisons

-- | Heroes cannot gain resources from card effects.
-- Travel: Each player must pay 1 resource to travel here.
instance HasModifiersFor GardenOfPoisons where
  getModifiersFor (GardenOfPoisons a) = do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotGainResourcesFromCardEffects]
    players <- getRemainingPlayers
    broke <- anyM (\p -> not <$> selectAny (HeroControlledBy (PlayerWithId p) <> HeroWithResources)) players
    when broke $ modified a a [CannotBeTraveledTo]

instance RunMessage GardenOfPoisons where
  runMessage msg l@(GardenOfPoisons a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer $ discardResourceFromAHero a
      pure l
    _ -> GardenOfPoisons <$> runMessage msg a
