module LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros (theSiegeOfCairAndros) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheSiegeOfCairAndros = TheSiegeOfCairAndros ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSiegeOfCairAndros :: Scenario
theSiegeOfCairAndros =
  Scenario
    $ scenario (TheSiegeOfCairAndros . (nightmareDeckL ?~ nightmare)) "the-siege-of-cair-andros" "The Siege of Cair Andros"
 where
  -- its setup card's text, player cards scoring nothing, is the victory points' own
  -- (see 'LordOfTheRings.Scenario.Types')
  nightmare =
    nightmareDeck
      Nightmare.theSiegeOfCairAndrosNightmare
      EncounterSet.TheSiegeOfCairAndrosNightmare
      [ (Enemies.orcArbalesters, 3)
      , (Treacheries.thePowerOfMordor, 1)
      , (Enemies.orcRabble, 2)
      , (Treacheries.scourgeOfMordor, 2)
      , (Enemies.southronMercenaries, 3)
      ]

instance RunMessage TheSiegeOfCairAndros where
  runMessage msg (TheSiegeOfCairAndros attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheSiegeOfCairAndros, EncounterSet.MordorElite, EncounterSet.RavagingOrcs, EncounterSet.Southrons]
      firstStage <- genEncounterCard Quests.theDefense
      laterStages <-
        traverse
          genEncounterCard
          [Quests.reinforcingTheBanks, Quests.breakthroughAtTheApproach, Quests.breakthroughAtTheCitadel, Quests.theLastBattle]
      putQuestIntoPlay firstStage
      pure $ TheSiegeOfCairAndros $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheSiegeOfCairAndros <$> runMessage msg attrs
