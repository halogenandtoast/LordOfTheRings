module LordOfTheRings.Location.Cards.CityForum (cityForum) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CityForum = CityForum LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

cityForum :: LocationCard CityForum
cityForum = location CityForum Cards.cityForum

-- | City Forum gets +1 Threat for each resource here.
instance HasModifiersFor CityForum where
  getModifiersFor (CityForum a) = do
    let n = countTokens Resource a.locationTokens
    when (n > 0) $ modifySelf a [ThreatModifier n]

-- | While City Forum is in the staging area, it gains: "Forced: After a location is
-- explored, place 1 resource here."
instance HasAbilities CityForum where
  getAbilities (CityForum a) = [mkAbility a 1 $ forced $ AnyExploredWindow After | a.placement == StagingArea]

-- | Travel: Each player raises his threat by 1 for each resource here.
instance RunMessage CityForum where
  runMessage msg l@(CityForum a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      pure l
    TravelTo lid | lid == a.id -> do
      let n = countTokens Resource a.locationTokens
      when (n > 0) $ eachPlayer (`raiseThreat` n)
      pure l
    _ -> CityForum <$> runMessage msg a
