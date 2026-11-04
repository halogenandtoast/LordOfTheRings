module LordOfTheRings.Location.Cards.TrollCave (trollCave) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (bilboHas)

newtype TrollCave = TrollCave LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

trollCave :: LocationCard TrollCave
trollCave = location TrollCave Cards.trollCave

-- | Immune to player card effects. Players cannot travel to Troll Cave unless Bilbo
-- Baggins has the Troll Key attached and the first player spends 5 Baggins resources. (2
-- Baggins resources instead if Bilbo Baggins has the Troll Purse attached.)
instance HasModifiersFor TrollCave where
  getModifiersFor (TrollCave a) = do
    key <- bilboHas Objectives.trollKey
    cost <- travelCost
    resources <- getBagginsResources
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | not key || resources < cost]

-- | The first player spends the Baggins resources from Bilbo Baggins' pool as the players
-- travel here.
instance RunMessage TrollCave where
  runMessage msg (TrollCave a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      cost <- travelCost
      traverse_ (\b -> spendResourcesFrom lead b cost) =<< getBilbo
      TrollCave <$> runMessage msg a
    _ -> TrollCave <$> runMessage msg a

travelCost :: HasGame m => m Int
travelCost = (\purse -> if purse then 2 else 5) <$> bilboHas Objectives.trollPurse
