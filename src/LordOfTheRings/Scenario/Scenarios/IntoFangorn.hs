module LordOfTheRings.Scenario.Scenarios.IntoFangorn (intoFangorn) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (huorn)

newtype IntoFangorn = IntoFangorn ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoFangorn :: Scenario
intoFangorn = Scenario $ scenario IntoFangorn "into-fangorn" "Into Fangorn"

-- | Stage 1 advances to stage 2, which wins the game; either stage runs out of time to
-- stage 3, which goes back to stage 2: the quest deck holds stage 2 alone, and the stages
-- reset to the others.
instance RunMessage IntoFangorn where
  runMessage msg (IntoFangorn attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.IntoFangorn, EncounterSet.AncientForest, EncounterSet.WearyTravelers]
      firstStage <- genEncounterCard Quests.intoTheWoods
      laterStages <- traverse genEncounterCard [Quests.escapeFromFangorn]
      putQuestIntoPlay firstStage
      pure $ IntoFangorn $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- The Forest's Malice: the player puts a Huorn enemy from the encounter deck or discard
    -- pile into play engaged with him, then shuffles the encounter deck
    ScenarioSpecific "intoFangorn.huornEngaged" (parseMaybe parseJSON -> Just pid) -> do
      let fromDeck = filter (cardMatches huorn) attrs.encounterDeck
          fromDiscard = filter (cardMatches huorn) attrs.encounterDiscard
      chooseOneM pid do
        questionLabeled "intoFangorn.huornEngaged"
        showing (map EncounterCard $ attrs.encounterDeck <> attrs.encounterDiscard)
        targets fromDeck \c -> removeFromEncounterDeck c >> putEnemyIntoPlayEngaged pid c
        targets fromDiscard \c -> removeFromEncounterDiscard c >> putEnemyIntoPlayEngaged pid c
      shuffleEncounterDeck
      pure $ IntoFangorn attrs
    -- The Angry Forest: the player reveals an enemy or objective from the discard pile
    ScenarioSpecific "intoFangorn.revealFromDiscard" (parseMaybe parseJSON -> Just pid) -> do
      let choices = filter (cardMatches $ CardMatchAny [CardWithType EnemyType, CardWithType ObjectiveType]) attrs.encounterDiscard
      chooseOneM pid do
        questionLabeled "intoFangorn.revealFromDiscard"
        showing (map EncounterCard attrs.encounterDiscard)
        targets choices revealFromEncounterDiscard
      pure $ IntoFangorn attrs
    _ -> IntoFangorn <$> runMessage msg attrs
