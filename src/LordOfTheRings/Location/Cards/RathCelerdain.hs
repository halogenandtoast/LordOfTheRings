module LordOfTheRings.Location.Cards.RathCelerdain (rathCelerdain) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (revealUnderworldCard)

newtype RathCelerdain = RathCelerdain LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rathCelerdain :: LocationCard RathCelerdain
rathCelerdain = location RathCelerdain Cards.rathCelerdain

-- | While Rath Celerdain is in the staging area it gains: "Forced: At the beginning of the
-- encounter phase, reveal the top card of the underworld deck. If it is an objective,
-- discard it." Its objectives are the Clues, which the reveal discards.
instance HasAbilities RathCelerdain where
  getAbilities (RathCelerdain a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase | a.placement == StagingArea]

instance RunMessage RathCelerdain where
  runMessage msg l@(RathCelerdain a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      revealUnderworldCard
      pure l
    _ -> RathCelerdain <$> runMessage msg a
