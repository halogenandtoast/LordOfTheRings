module LordOfTheRings.Location.Cards.BrakeOfThorns (brakeOfThorns) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype BrakeOfThorns = BrakeOfThorns LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

brakeOfThorns :: LocationCard BrakeOfThorns
brakeOfThorns = location BrakeOfThorns Cards.brakeOfThorns

-- | While Brake of Thorns is in the staging area, add 1 Eye icon to each Fortitude test.
instance HasModifiersFor BrakeOfThorns where
  getModifiersFor (BrakeOfThorns a) = when (a.placement == StagingArea) $ modified a GameTarget [FortitudeTestEyes 1]

-- | Forced: After Brake of Thorns becomes the active location, each player assigns X damage
-- among characters he controls, where X is the tens digit of his threat dial.
instance HasAbilities BrakeOfThorns where
  getAbilities (BrakeOfThorns a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage BrakeOfThorns where
  runMessage msg l@(BrakeOfThorns a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        x <- (`div` 10) <$> getThreat pid
        characters <- charactersControlledBy pid
        when (x > 0 && notNull characters) $ dealDamageDividedAmong a pid characters x
      pure l
    _ -> BrakeOfThorns <$> runMessage msg a
