module LordOfTheRings.Objective.Cards.TowerOfTheHeron (towerOfTheHeron) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (controlledByPlayers)

newtype TowerOfTheHeron = TowerOfTheHeron ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

towerOfTheHeron :: ObjectiveCard TowerOfTheHeron
towerOfTheHeron = objective TowerOfTheHeron Cards.towerOfTheHeron

-- | Guarded. If Tower of the Heron is free of encounters, add it to the staging area,
-- controlled by the players. While Tower of the Heron is controlled by the players,
-- reduce the archery total by 4.
instance HasModifiersFor TowerOfTheHeron where
  getModifiersFor (TowerOfTheHeron a) = whenM (controlledByPlayers a) $ modified a GameTarget [ArcheryBonus (-4)]
