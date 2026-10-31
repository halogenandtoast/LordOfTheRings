module LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor (theBloodOfGondor) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (PlayersCannotWin))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheBloodOfGondor = TheBloodOfGondor ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBloodOfGondor :: Scenario
theBloodOfGondor =
  Scenario
    $ scenario (TheBloodOfGondor . (nightmareDeckL ?~ nightmare)) "the-blood-of-gondor" "The Blood of Gondor"
 where
  -- the nightmare Black Númenórean takes the place of the standard one
  nightmare =
    nightmareDeck
      Nightmare.theBloodOfGondorNightmare
      EncounterSet.TheBloodOfGondorNightmare
      [ (Enemies.orcRabble, 1)
      , (Enemies.orcArsonist, 1)
      , (Treacheries.scourgeOfMordor, 2)
      , (Enemies.blackNumenorean, 1)
      , (Enemies.evilCrow, 2)
      , (Locations.theDarkWoods, 4)
      , (Treacheries.lyingInWait, 2)
      ]

-- | Nightmare: "The players cannot win the game unless The Crossroads and Black Númenórean
-- are in the victory display."
instance HasModifiersFor TheBloodOfGondor where
  getModifiersFor (TheBloodOfGondor a) = when a.nightmare do
    let victory = map toCardCode a.victoryDisplay
        needed = map toCardCode [Locations.theCrossroads, Enemies.blackNumenoreanNightmare]
    unless (all (`elem` victory) needed) $ modified a GameTarget [PlayersCannotWin]

instance RunMessage TheBloodOfGondor where
  runMessage msg (TheBloodOfGondor attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.TheBloodOfGondor, EncounterSet.RavagingOrcs]
      firstStage <- genEncounterCard Quests.theAmbush
      laterStages <- traverse genEncounterCard [Quests.captured]
      putQuestIntoPlay firstStage
      pure $ TheBloodOfGondor $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Setup: Each player takes 1 hidden card."
    DoStep 8 Setup | attrs.nightmare -> do
      eachPlayer takeHiddenCard
      pure $ TheBloodOfGondor attrs
    _ -> TheBloodOfGondor <$> runMessage msg attrs
