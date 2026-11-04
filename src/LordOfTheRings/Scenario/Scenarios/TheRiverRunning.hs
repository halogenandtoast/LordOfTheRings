module LordOfTheRings.Scenario.Scenarios.TheRiverRunning (theRiverRunning) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheRiverRunning = TheRiverRunning ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRiverRunning :: Scenario
theRiverRunning = Scenario $ scenario TheRiverRunning "the-river-running" "The River Running"

{- | The encounter deck is built from The River Running, Riders of Rhûn, Easterling Raiders
and Rolling Plains (MEC77 rules). Stage 1A's "Set Warriors of the East aside, out of play"
is done as the deck is gathered.
-}
instance RunMessage TheRiverRunning where
  runMessage msg (TheRiverRunning attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheRiverRunning, EncounterSet.RidersOfRhun, EncounterSet.EasterlingRaiders, EncounterSet.RollingPlains]
      let (aside, encounterDeck) = partition (cardMatches $ cardIs Quests.warriorsOfTheEast) cards
      firstStage <- genEncounterCard Quests.chasedByEasterlings
      laterStages <- traverse genEncounterCard [Quests.theCrossingOfAraw]
      putQuestIntoPlay firstStage
      pure
        $ TheRiverRunning
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    _ -> TheRiverRunning <$> runMessage msg attrs
