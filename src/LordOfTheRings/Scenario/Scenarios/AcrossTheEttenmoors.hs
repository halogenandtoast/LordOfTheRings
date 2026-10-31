module LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors (acrossTheEttenmoors) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype AcrossTheEttenmoors = AcrossTheEttenmoors ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

acrossTheEttenmoors :: Scenario
acrossTheEttenmoors =
  Scenario
    $ scenario (AcrossTheEttenmoors . (nightmareDeckL ?~ nightmare)) "across-the-ettenmoors" "Across the Ettenmoors"
 where
  -- its setup card's Forced, a safe location guarding each side quest stages 2A and 3A
  -- reveal, is theirs; its stage 1 replaces the standard one
  nightmare =
    nightmareDeck
      Nightmare.acrossTheEttenmoorsNightmare
      EncounterSet.AcrossTheEttenmoorsNightmare
      [ (Quests.intoTheEttenmoors, 1)
      , (Locations.shroudedHills, 2)
      , (Treacheries.noRest, 1)
      , (Treacheries.aradorsBane, 2)
      , (Locations.trollFells, 2)
      , (Enemies.goblinPursuer, 2)
      , (Treacheries.coldFromAngmar, 2)
      ]

-- | The encounter deck is built from the Across the Ettenmoors, Eriador Wilds and Foul
-- Weather sets. The safe keyword's rules are kept by the safe locations themselves.
instance RunMessage AcrossTheEttenmoors where
  runMessage msg (AcrossTheEttenmoors attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.AcrossTheEttenmoors, EncounterSet.EriadorWilds, EncounterSet.FoulWeather]
      firstStage <- genEncounterCard $ nightmareOr attrs Quests.intoTheEttenmoors Quests.intoTheEttenmoorsNightmare
      laterStages <- traverse genEncounterCard [Quests.aMiserableJourney, Quests.journeysEnd]
      putQuestIntoPlay firstStage
      pure $ AcrossTheEttenmoors $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> AcrossTheEttenmoors <$> runMessage msg attrs
