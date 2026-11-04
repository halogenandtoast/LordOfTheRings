module LordOfTheRings.Scenario.Scenarios.BeneathTheSands (beneathTheSands) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype BeneathTheSands = BeneathTheSands ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beneathTheSands :: Scenario
beneathTheSands = Scenario $ scenario BeneathTheSands "beneath-the-sands" "Beneath the Sands"

{- | Built from the Beneath the Sands, Desert Creatures and Harad Territory encounter sets
(rule sheet). The Search objective, Off Track faceup, and Brood Mother come out of the
encounter deck as the first stage is set up. The Track keyword and the two sides of the
Search objective are its cards' ("LordOfTheRings.Objective.Cards.OnTrack").
-}
instance RunMessage BeneathTheSands where
  runMessage msg (BeneathTheSands attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.BeneathTheSands, EncounterSet.DesertCreatures, EncounterSet.HaradTerritory]
      firstStage <- genEncounterCard Quests.searchingTheCaves
      laterStages <- traverse genEncounterCard [Quests.gettingCloser, Quests.theSpidersHive]
      putQuestIntoPlay firstStage
      pure $ BeneathTheSands $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> BeneathTheSands <$> runMessage msg attrs
