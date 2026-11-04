module LordOfTheRings.Scenario.Scenarios.EscapeFromUmbar (escapeFromUmbar) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (getMainQuests)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype EscapeFromUmbar = EscapeFromUmbar ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromUmbar :: Scenario
escapeFromUmbar = Scenario $ scenario EscapeFromUmbar "escape-from-umbar" "Escape from Umbar"

{- | The encounter deck is built from Escape from Umbar, Harad Soldiers and Harad Territory
(The Sands of Harad rules insert). Stranded in Umbar's setup does the rest.

The scenario notes the main quest in play as each round begins, for Seize Them!'s "if the
main quest has been in play the entire round".
-}
instance RunMessage EscapeFromUmbar where
  runMessage msg (EscapeFromUmbar attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.EscapeFromUmbar, EncounterSet.HaradSoldiers, EncounterSet.HaradTerritory]
      firstStage <- genEncounterCard Quests.strandedInUmbar
      laterStages <- traverse genEncounterCard [Quests.fleeTheCity]
      putQuestIntoPlay firstStage
      pure $ EscapeFromUmbar $ attrs & encounterDeckL .~ cards & questDeckL .~ laterStages
    BeginRound -> do
      mains <- getMainQuests
      EscapeFromUmbar <$> runMessage msg (attrs & metaL .~ toJSON mains)
    _ -> EscapeFromUmbar <$> runMessage msg attrs
