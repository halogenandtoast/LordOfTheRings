module LordOfTheRings.Location.Cards.HennethAnnun (hennethAnnun) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (getTheBlackGate)

newtype HennethAnnun = HennethAnnun LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hennethAnnun :: LocationCard HennethAnnun
hennethAnnun = location HennethAnnun Cards.hennethAnnun

-- | While Henneth Annûn is the active location, skip both the encounter phase and combat
-- phase, and ignore the Forced effects on stage 1B and The Black Gate.
instance HasModifiersFor HennethAnnun where
  getModifiersFor (HennethAnnun a) = when (a.placement == ActiveLocation) do
    modified a GameTarget [SkipPhase EncounterPhase, SkipPhase CombatPhase]
    quests <- getMainQuests
    for_ quests \q -> modified a q [CannotTriggerAbilities]
    gate <- getTheBlackGate
    for_ gate \g -> modified a g [CannotTriggerAbilities]

-- | Forced: After Henneth Annûn becomes the active location, return each engaged enemy to
-- the staging area.
instance HasAbilities HennethAnnun where
  getAbilities (HennethAnnun a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage HennethAnnun where
  runMessage msg l@(HennethAnnun a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnEngagedEnemiesToStaging
      pure l
    _ -> HennethAnnun <$> runMessage msg a
