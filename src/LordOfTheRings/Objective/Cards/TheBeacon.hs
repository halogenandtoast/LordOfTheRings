module LordOfTheRings.Objective.Cards.TheBeacon (theBeacon) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype TheBeacon = TheBeacon ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBeacon :: ObjectiveCard TheBeacon
theBeacon = objective TheBeacon Cards.theBeacon

-- | Guarded. If The Beacon is free of encounters, add it to the staging area, controlled
-- by the players. Planning Action: If the players control The Beacon, exhaust it and spend
-- 5 resources to take control of 1 set aside copy of Dol Amroth Warship. Any player may
-- trigger this action.
instance HasAbilities TheBeacon where
  getAbilities (TheBeacon a) =
    [ anyPlayer
        $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> DuringPhase PlanningPhase)
        $ ActionAbility (Costs [exhaustCost a, SpendResourcesCost Neutral 5])
    ]

instance RunMessage TheBeacon where
  runMessage msg o@(TheBeacon attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      outOfPlay <- getsGame (scenarioSetAside . toAttrs . gameScenario)
      for_ (find (cardMatches $ cardIs Cards.dolAmrothWarship) outOfPlay) (putShipIntoPlayUnder pid)
      pure o
    _ -> TheBeacon <$> runMessage msg attrs
