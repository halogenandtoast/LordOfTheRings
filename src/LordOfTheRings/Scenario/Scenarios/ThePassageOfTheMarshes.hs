module LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes (thePassageOfTheMarshes) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (getRemainingPlayers)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (passageOfTheMarshesResolution)
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype ThePassageOfTheMarshes = ThePassageOfTheMarshes ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePassageOfTheMarshes :: Scenario
thePassageOfTheMarshes = Scenario $ scenario ThePassageOfTheMarshes "the-passage-of-the-marshes" "The Passage of the Marshes"

{- | The Passage of the Marshes, Gollum and Morgul Nazgûl make up the encounter deck (The
Land of Shadow rules insert). Gollum is not gathered, having no encounter card back: stage
1A puts him into play.
-}
instance RunMessage ThePassageOfTheMarshes where
  runMessage msg (ThePassageOfTheMarshes attrs) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets attrs [EncounterSet.ThePassageOfTheMarshes, EncounterSet.GollumSaga, EncounterSet.NazgulSaga]
      firstStage <- genEncounterCard Quests.theTamingOfSmeagol
      rest <- traverse genEncounterCard [Quests.acrossTheMarshes, Quests.dontLookAtTheLights]
      putQuestIntoPlay firstStage
      pure $ ThePassageOfTheMarshes $ attrs & encounterDeckL .~ deck & questDeckL .~ rest
    -- Campaign: Setup: Each player shuffles 1 copy of The Searching Eye into his deck. (The
    -- burdens of Helm's Deep and The Road to Isengard are left out as the deck is dealt: see
    -- 'LordOfTheRings.Saga.removedBurdenSets'.)
    DoStep 9 Setup | isJust attrs.campaign -> do
      players <- getRemainingPlayers
      for_ players \pid -> shuffleIntoDeck pid =<< genEncounterCard Treacheries.theSearchingEye
      ThePassageOfTheMarshes <$> runMessage msg attrs
    ResolveCampaign | isJust attrs.campaign -> do
      passageOfTheMarshesResolution
      ThePassageOfTheMarshes <$> runMessage msg attrs
    _ -> ThePassageOfTheMarshes <$> runMessage msg attrs
