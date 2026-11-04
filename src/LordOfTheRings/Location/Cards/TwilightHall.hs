module LordOfTheRings.Location.Cards.TwilightHall (twilightHall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Forest))

newtype TwilightHall = TwilightHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

twilightHall :: LocationCard TwilightHall
twilightHall = location TwilightHall Cards.twilightHall

resourcesOn :: LocationAttrs -> Int
resourcesOn a = countTokens Resource a.locationTokens

-- | Twilight Hall gets +2 threat for each resource on it. If there are no resources on
-- Twilight Hall, the players cannot travel here.
instance HasModifiersFor TwilightHall where
  getModifiersFor (TwilightHall a) = do
    let n = resourcesOn a
    when (n > 0) $ modifySelf a [ThreatModifier (2 * n)]
    when (n == 0) $ modifySelf a [CannotBeTraveledTo]

-- | Forced: After a Forest location is explored, place 1 resource here.
instance HasAbilities TwilightHall where
  getAbilities (TwilightHall a) = [mkAbility a 1 $ forced $ ExploredMatchingWindow After (CardWithTrait Forest)]

-- | Travel: Raise each player's threat by 1 for each resource here.
instance RunMessage TwilightHall where
  runMessage msg l@(TwilightHall a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      pure l
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> raiseThreat p (resourcesOn a)
      TwilightHall <$> runMessage msg a
    _ -> TwilightHall <$> runMessage msg a
