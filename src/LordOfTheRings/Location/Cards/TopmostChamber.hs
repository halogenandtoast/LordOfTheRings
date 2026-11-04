module LordOfTheRings.Location.Cards.TopmostChamber (topmostChamber) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (atStage)

newtype TopmostChamber = TopmostChamber LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Ring-bearer is guarded under it from stage 1A.
topmostChamber :: LocationCard TopmostChamber
topmostChamber = location TopmostChamber Cards.topmostChamber

-- | Immune to player card effects. The players cannot travel here unless there is at least
-- 18 progress on stage 2B.
instance HasModifiersFor TopmostChamber where
  getModifiersFor (TopmostChamber a) = do
    reachable <- atStage 2 18
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | not reachable]

-- | Travel: Add Shagrat to the staging area to travel here.
instance RunMessage TopmostChamber where
  runMessage msg (TopmostChamber a) = case msg of
    TravelTo lid | lid == a.id -> do
      addSetAsideToStaging (cardIs Enemies.shagrat)
      TopmostChamber <$> runMessage msg a
    _ -> TopmostChamber <$> runMessage msg a
