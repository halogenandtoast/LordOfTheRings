module LordOfTheRings.Location.Cards.DarkPools (darkPools) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DarkPools = DarkPools LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkPools :: LocationCard DarkPools
darkPools = location DarkPools Cards.darkPools

-- | Progress cannot be placed on Dark Pools while it is in the staging area.
instance HasModifiersFor DarkPools where
  getModifiersFor (DarkPools a) = when (a.placement == StagingArea) $ modifySelf a [ProgressLimit 0]

instance HasAbilities DarkPools where
  getAbilities (DarkPools a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Forced: After Dark Pools becomes the active location, each player must discard an ally
-- with the highest printed cost among the allies he controls.
instance RunMessage DarkPools where
  runMessage msg l@(DarkPools a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        allies <- traverse getAlly =<< select (AllyControlledBy $ PlayerWithId pid)
        let highest = foldr (max . printedCostOf) 0 allies
        chooseOrRunOneM pid do
          questionLabeled "darkPools.discard"
          causedBy a
          targets [toId x | x <- allies, printedCostOf x == highest] (discard a)
      pure l
    _ -> DarkPools <$> runMessage msg a
