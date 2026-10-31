module LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen (theFordsOfIsen) where

import LordOfTheRings.Ability (forced, mkAbility)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher.Window (WindowMatcher (AdvancedToStageWindow))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen.Helpers (handSize, runFordsOfIsenStep)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After))

newtype TheFordsOfIsen = TheFordsOfIsen ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFordsOfIsen :: Scenario
theFordsOfIsen =
  Scenario
    $ scenario (TheFordsOfIsen . (nightmareDeckL ?~ nightmare)) "the-fords-of-isen" "The Fords of Isen"
 where
  nightmare =
    nightmareDeck
      Nightmare.theFordsOfIsenNightmare
      EncounterSet.TheFordsOfIsenNightmare
      [ (Enemies.dunlandProwler, 3)
      , (Enemies.dunlendingBandit, 2)
      , (Enemies.dunlandBerserker, 1)
      , (Locations.theKingsRoad, 2)
      , (Locations.gapOfRohan, 2)
      ]

-- | Nightmare: "Forced: After the players advance to a new quest stage, each player with
-- fewer than 3 cards in his hand draws 3 cards."
instance HasAbilities TheFordsOfIsen where
  getAbilities (TheFordsOfIsen a) = [mkAbility a 1 $ forced $ AdvancedToStageWindow After | a.nightmare]

-- | The Fords of Isen, Dunland Raiders and Dunland Warriors make up the encounter deck.
-- The rules sheet bars the Gríma hero from this scenario: see 'barredHeroes'.
instance RunMessage TheFordsOfIsen where
  runMessage msg (TheFordsOfIsen attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheFordsOfIsen, EncounterSet.DunlandRaiders, EncounterSet.DunlandWarriors]
      firstStage <- genEncounterCard Quests.fightAtTheFords
      laterStages <- traverse genEncounterCard [Quests.dunlendingAttack, Quests.holdTheFords]
      putQuestIntoPlay firstStage
      pure $ TheFordsOfIsen $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    UseCardAbility _ ScenarioSource 1 _ -> do
      eachPlayer \p -> do
        n <- handSize p
        when (n < 3) $ drawCards attrs p 3
      pure $ TheFordsOfIsen attrs
    ScenarioSpecific key v -> do
      runFordsOfIsenStep key v
      pure $ TheFordsOfIsen attrs
    _ -> TheFordsOfIsen <$> runMessage msg attrs
