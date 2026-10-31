module LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador (theWastesOfEriador) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheWastesOfEriador = TheWastesOfEriador ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWastesOfEriador :: Scenario
theWastesOfEriador =
  Scenario
    $ scenario (TheWastesOfEriador . (nightmareDeckL ?~ nightmare)) "the-wastes-of-eriador" "The Wastes of Eriador"
 where
  nightmare =
    nightmareDeck
      Nightmare.theWastesOfEriadorNightmare
      EncounterSet.TheWastesOfEriadorNightmare
      [ (Enemies.northernWarg, 2)
      , (Enemies.whiteWarg, 1)
      , (Treacheries.suddenDarkness, 1)
      , (Locations.northDowns, 1)
      , (Locations.eriadorWastes, 2)
      , (Treacheries.coldFromAngmar, 1)
      , (Treacheries.bitingWind, 3)
      , (Treacheries.freezingBlast, 1)
      , (Locations.shroudedHills, 1)
      , (Locations.ruggedCountry, 3)
      , (Treacheries.pressingNeeds, 2)
      ]

-- | Built from The Wastes of Eriador, Eriador Wilds and Foul Weather encounter sets. The
-- Time objective, Daybreak or Nightfall faceup, says whether it is Day or Night.
instance RunMessage TheWastesOfEriador where
  runMessage msg (TheWastesOfEriador attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheWastesOfEriador, EncounterSet.EriadorWilds, EncounterSet.FoulWeather]
      firstStage <- genEncounterCard Quests.acrossTheWastes
      laterStages <- traverse genEncounterCard [Quests.howlingAtNight, Quests.battleWithThePack]
      putQuestIntoPlay firstStage
      pure $ TheWastesOfEriador $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheWastesOfEriador <$> runMessage msg attrs
