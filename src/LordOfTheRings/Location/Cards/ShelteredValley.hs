module LordOfTheRings.Location.Cards.ShelteredValley (shelteredValley) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (getMissionObjective, missionResources)

newtype ShelteredValley = ShelteredValley LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

shelteredValley :: LocationCard ShelteredValley
shelteredValley = location ShelteredValley Cards.shelteredValley

-- | Travel: Remove 1 resource token from the Mission objective. Without one to remove, the
-- players cannot travel here.
instance HasModifiersFor ShelteredValley where
  getModifiersFor (ShelteredValley a) = do
    n <- missionResources
    when (n == 0) $ modified a a [CannotBeTraveledTo]

-- | While Sheltered Valley is the active location, it gains: "Response: When Sheltered
-- Valley is explored, heal all damage from 1 character."
instance HasAbilities ShelteredValley where
  getAbilities (ShelteredValley a) = [mkAbility a 1 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]

instance RunMessage ShelteredValley where
  runMessage msg l@(ShelteredValley a) = case msg of
    TravelTo lid | lid == a.id -> do
      traverse_ (\m -> removeTokens a m Resource 1) =<< getMissionObjective
      ShelteredValley <$> runMessage msg a
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      chooseOrRunOneM pid do
        questionLabeled "shelteredValley.heal"
        causedBy a
        targets damaged (healAllDamage a)
      pure l
    _ -> ShelteredValley <$> runMessage msg a
