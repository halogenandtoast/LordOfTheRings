module LordOfTheRings.Location.Cards.MountDoom (mountDoom) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (atStage)

newtype MountDoom = MountDoom LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountDoom :: LocationCard MountDoom
mountDoom = location MountDoom Cards.mountDoom

-- | Immune to player card effects. X is the tens digit of the first player's threat dial.
-- The players cannot travel here unless there is at least 20 progress on stage 3B.
instance HasModifiersFor MountDoom where
  getModifiersFor (MountDoom a) = do
    threat <- getThreat =<< getLead
    reachable <- atStage 3 20
    modifySelf a $ [ImmuneToPlayerCardEffects, ThreatModifier (threat `div` 10)] <> [CannotBeTraveledTo | not reachable]

-- | Travel: Each player makes a Fortitude test. If a player fails this test, raise his
-- threat by 9.
instance RunMessage MountDoom where
  runMessage msg (MountDoom a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \pid -> fortitudeTest pid a (encounterRaisesThreat pid 9) (pure ())
      MountDoom <$> runMessage msg a
    _ -> MountDoom <$> runMessage msg a
