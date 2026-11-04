module LordOfTheRings.Location.Cards.StrongholdTower (strongholdTower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype StrongholdTower = StrongholdTower LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

strongholdTower :: LocationCard StrongholdTower
strongholdTower = location StrongholdTower Cards.strongholdTower

-- | While Stronghold Tower is in the staging area, shadow effects cannot be canceled.
instance HasModifiersFor StrongholdTower where
  getModifiersFor (StrongholdTower a) = when (a.placement == StagingArea) $ modified a GameTarget [ShadowCannotBeCanceled]

-- | Travel: Each player must deal 2 damage to a character he controls to travel here.
instance RunMessage StrongholdTower where
  runMessage msg l@(StrongholdTower a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> dealDamageToOneCharacter a p 2
      pure l
    _ -> StrongholdTower <$> runMessage msg a
