module LordOfTheRings.Scenario.Scenarios.TheSeventhLevel (theSeventhLevel) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), LocationMatcher (IsActiveLocation), cardIs, cardMatches, select)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Types (QuestAttrs (questFlipped))
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheSeventhLevel = TheSeventhLevel ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSeventhLevel :: Scenario
theSeventhLevel = Scenario $ scenario (TheSeventhLevel . (nightmareDeckL ?~ nightmare)) "the-seventh-level" "The Seventh Level"
 where
  nightmare =
    nightmareDeck
      Nightmare.theSeventhLevelNightmare
      EncounterSet.TheSeventhLevelNightmare
      [ (Enemies.goblinScout, 2)
      , (Enemies.goblinSwordsman, 2)
      , (Enemies.goblinSpearman, 2)
      , (Locations.plunderedArmoury, 2)
      , (Treacheries.hiddenThreat, 2)
      , (Locations.upperHall, 2)
      ]

instance RunMessage TheSeventhLevel where
  runMessage msg (TheSeventhLevel attrs) = case msg of
    -- Nightmare: "Setup: Set Overseer Maurûl and Chamber of Records aside, out of play."
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheSeventhLevel, EncounterSet.PlunderingGoblins, EncounterSet.GoblinsOfTheDeep]
      let (aside, encounterDeck) = partition (cardMatches $ CardMatchAny [cardIs Enemies.overseerMaurul, cardIs Locations.chamberOfRecords]) cards
      firstStage <- genEncounterCard Quests.searchForTheChamber
      laterStages <- traverse genEncounterCard [Quests.theFateOfBalin]
      putQuestIntoPlay firstStage
      pure
        $ TheSeventhLevel
        $ attrs
        & encounterDeckL .~ encounterDeck
        & setAsideL %~ (<> aside)
        & questDeckL .~ laterStages
    -- Nightmare: "Forced: When stage 2B is revealed, make Chamber of Records the active
    -- location (returning any currently active location to the staging area) and add
    -- Overseer Maurûl to the staging area. Then, shuffle all copies of Balin's Doom in the
    -- encounter discard pile back into the encounter deck."
    Flip (QuestTarget qid) | attrs.nightmare -> do
      revealed <-
        getsGame
          $ maybe False (\q -> toCardCode q == toCardCode Quests.theFateOfBalin && not (toAttrs q).questFlipped)
          . lookup qid
          . (.entities.quests)
      when revealed do
        active <- select IsActiveLocation
        for_ active \l -> placeLocation l StagingArea
        for_ (find (cardMatches $ cardIs Locations.chamberOfRecords) attrs.scenarioSetAside) putIntoPlayAsActiveLocation
        addSetAsideToStaging (cardIs Enemies.overseerMaurul)
        shuffleEncounterDiscardIntoDeck (cardIs Treacheries.balinsDoom)
      TheSeventhLevel <$> runMessage msg attrs
    _ -> TheSeventhLevel <$> runMessage msg attrs
