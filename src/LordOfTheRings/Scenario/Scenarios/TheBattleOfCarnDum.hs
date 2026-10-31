module LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum (theBattleOfCarnDum) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheBattleOfCarnDum = TheBattleOfCarnDum ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfCarnDum :: Scenario
theBattleOfCarnDum =
  Scenario
    $ scenario (TheBattleOfCarnDum . (nightmareDeckL ?~ nightmare)) "the-battle-of-carn-dum" "The Battle of Carn Dûm"
 where
  -- its setup card only explains the Siege keyword, Protect the Flanks'
  nightmare =
    nightmareDeck
      Nightmare.theBattleOfCarnDumNightmare
      EncounterSet.TheBattleOfCarnDumNightmare
      [ (Treacheries.vileAffliction, 1)
      , (Quests.furiousCharge, 1)
      , (Quests.orcAmbush, 1)
      , (Enemies.angmarOrc, 1)
      , (Enemies.angmarMarauder, 3)
      , (Treacheries.terrorOfTheNorth, 3)
      , (Locations.accursedBattlefield, 2)
      , (Locations.blightOfCarnDum, 1)
      ]

-- | The encounter deck is built from The Battle of Carn Dûm, Angmar Orcs and Dark Sorcery.
instance RunMessage TheBattleOfCarnDum where
  runMessage msg (TheBattleOfCarnDum attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheBattleOfCarnDum, EncounterSet.AngmarOrcs, EncounterSet.DarkSorcery]
      firstStage <- genEncounterCard Quests.theClutchesOfCarnDum
      laterStages <- traverse genEncounterCard [Quests.midwintersCrux]
      putQuestIntoPlay firstStage
      pure $ TheBattleOfCarnDum $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheBattleOfCarnDum <$> runMessage msg attrs
