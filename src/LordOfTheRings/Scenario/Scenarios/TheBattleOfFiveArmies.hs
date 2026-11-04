module LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies (theBattleOfFiveArmies) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (getLead, getMainQuests)
import LordOfTheRings.Modifier (ModifierType (ProgressLimit))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (chosenStage)
import LordOfTheRings.Window (Window (..), WindowType (RoundBegins))
import LordOfTheRings.Timing (Timing (When))

newtype TheBattleOfFiveArmies = TheBattleOfFiveArmies ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfFiveArmies :: Scenario
theBattleOfFiveArmies = Scenario $ scenario TheBattleOfFiveArmies "the-battle-of-five-armies" "The Battle of Five Armies"

-- | Progress cannot be placed on any quest stage except for the current quest stage (The
-- Hobbit: On the Doorstep rules insert, "Current Quest Stage").
instance HasModifiersFor TheBattleOfFiveArmies where
  getModifiersFor (TheBattleOfFiveArmies a) = do
    mains <- getMainQuests
    when (length mains > 1) do
      chosen <- chosenStage
      for_ mains \q -> when (Just q /= chosen) $ modified a q [ProgressLimit 0]

{- | The Battle of Five Armies and Wilderland make up the encounter deck (The Hobbit: On the
Doorstep rules insert). Stage 1 sets up the rest. The first player takes control of Bilbo
Baggins as the game sets up (see 'LordOfTheRings.Saga.fellowshipSetup').

At the beginning of each round, before players have collected resources, the first player
must decide which stage will be the current quest stage for that round (rules insert,
"Current Quest Stage"): asked as the round begins, while stages 2 to 4 are in play.

Nothing is discovered here: its cards name no treasure.
-}
instance RunMessage TheBattleOfFiveArmies where
  runMessage msg (TheBattleOfFiveArmies attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheBattleOfFiveArmies, EncounterSet.Wilderland]
      firstStage <- genEncounterCard Quests.theGatheringOfTheClouds
      laterStages <- traverse genEncounterCard [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill, Quests.theCloudsBurst]
      putQuestIntoPlay firstStage
      pure $ TheBattleOfFiveArmies $ attrs & encounterDeckL .~ cards & questDeckL .~ laterStages
    CheckWindows ws | Window When RoundBegins `elem` ws -> do
      mains <- getMainQuests
      when (length mains > 1) do
        lead <- getLead
        chooseOneM lead do
          questionLabeled "theBattleOfFiveArmies.currentQuest"
          targets mains setCurrentQuest
      TheBattleOfFiveArmies <$> runMessage msg attrs
    _ -> TheBattleOfFiveArmies <$> runMessage msg attrs
