module LordOfTheRings.Location.Cards.TarlangsNeck (tarlangsNeck) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TarlangsNeck = TarlangsNeck LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tarlangsNeck :: LocationCard TarlangsNeck
tarlangsNeck = location TarlangsNeck Cards.tarlangsNeck

instance HasAbilities TarlangsNeck where
  getAbilities (TarlangsNeck a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase | a.placement == ActiveLocation]

instance RunMessage TarlangsNeck where
  runMessage msg l@(TarlangsNeck a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      enemies <- select EnemyInStagingArea
      chooseOneM lead do
        questionLabeled "erech.neck"
        causedBy a
        labeled "erech.endEncounter" $ doStep 1 msg
        labeled "erech.engageAll" $ for_ enemies (engageEnemy lead)
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      endCurrentPhase
      pure l
    _ -> TarlangsNeck <$> runMessage msg a
