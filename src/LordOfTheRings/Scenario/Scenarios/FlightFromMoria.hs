module LordOfTheRings.Scenario.Scenarios.FlightFromMoria (flightFromMoria) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (getRemainingPlayers)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype FlightFromMoria = FlightFromMoria ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

flightFromMoria :: Scenario
flightFromMoria = Scenario $ scenario FlightFromMoria "flight-from-moria" "Flight from Moria"

instance RunMessage FlightFromMoria where
  runMessage msg (FlightFromMoria attrs) = case msg of
    -- A Presence in the Dark's setup keeps 1 copy of A Foe Beyond per player in the deck;
    -- the Search for an Exit stages are shuffled into the quest deck beneath it
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.FlightFromMoria, EncounterSet.DeepsOfMoria, EncounterSet.HazardsOfThePit, EncounterSet.PlunderingGoblins]
      players <- length <$> getRemainingPlayers
      let (foes, rest) = partition ((== toCardCode Treacheries.aFoeBeyond) . toCardCode) cards
      firstStage <- genEncounterCard Quests.aPresenceInTheDark
      laterStages <-
        shuffleM
          =<< traverse
            genEncounterCard
            [ Quests.headingDown
            , Quests.headingUp
            , Quests.aWrongTurn
            , Quests.narrowPaths
            , Quests.hastyCouncil
            , Quests.blockedByShadow
            , Quests.escapeFromDarkness
            ]
      putQuestIntoPlay firstStage
      pure $ FlightFromMoria $ attrs & encounterDeckL .~ take players foes <> rest & questDeckL .~ laterStages
    -- a Search for an Exit stage comes out on side A, and flips itself at the staging step
    AdvanceToQuest card -> do
      putQuestIntoPlay card
      pure $ FlightFromMoria attrs
    _ -> FlightFromMoria <$> runMessage msg attrs
