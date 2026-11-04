module LordOfTheRings.Location.Cards.HillsOfHollin (hillsOfHollin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype HillsOfHollin = HillsOfHollin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hillsOfHollin :: LocationCard HillsOfHollin
hillsOfHollin = location HillsOfHollin Cards.hillsOfHollin

-- | While Hills of Hollin is the active location, each enemy in the staging area gets +5
-- engagement cost.
instance HasModifiersFor HillsOfHollin where
  getModifiersFor (HillsOfHollin a) = when (a.placement == ActiveLocation) do
    enemies <- select EnemyInStagingArea
    for_ enemies \e -> modified a e [EngagementCostModifier 5]

-- | Forced: When Hills of Hollin is explored, each player raises his threat by 1 for each
-- damage here.
instance HasAbilities HillsOfHollin where
  getAbilities (HillsOfHollin a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage HillsOfHollin where
  runMessage msg l@(HillsOfHollin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid a.damage
      pure l
    _ -> HillsOfHollin <$> runMessage msg a
