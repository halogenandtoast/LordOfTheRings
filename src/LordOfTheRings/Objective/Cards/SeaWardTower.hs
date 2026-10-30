module LordOfTheRings.Objective.Cards.SeaWardTower (seaWardTower) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (controlledByPlayers)

newtype SeaWardTower = SeaWardTower ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

seaWardTower :: ObjectiveCard SeaWardTower
seaWardTower = objective SeaWardTower Cards.seaWardTower

-- | Guarded. If Sea-ward Tower is free of encounters, add it to the staging area,
-- controlled by the players. While the players control Sea-ward Tower, look at 2
-- additional cards from the top of the encounter deck during each Sailing test.
instance HasModifiersFor SeaWardTower where
  getModifiersFor (SeaWardTower a) = whenM (controlledByPlayers a) $ modified a GameTarget [SailingTestAdditionalCards 2]
