module LordOfTheRings.Scenario.Scenarios.IntrudersInChetwood (intrudersInChetwood) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype IntrudersInChetwood = IntrudersInChetwood ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intrudersInChetwood :: Scenario
intrudersInChetwood =
  Scenario
    $ scenario (IntrudersInChetwood . (nightmareDeckL ?~ nightmare)) "intruders-in-chetwood" "Intruders in Chetwood"
 where
  -- its setup card has no text of its own beyond the removals
  nightmare =
    nightmareDeck
      Nightmare.intrudersInChetwoodNightmare
      EncounterSet.IntrudersInChetwoodNightmare
      [ (Enemies.angmarOrc, 1)
      , (Enemies.angmarMarauder, 3)
      , (Locations.chetwoodForest, 2)
      , (Locations.outlyingHomestead, 1)
      , (Locations.bordersOfBreeLand, 2)
      , (Locations.ruggedCountry, 1)
      , (Locations.shroudedHills, 1)
      , (Treacheries.surprisingSpeed, 3)
      , (Treacheries.suddenAssault, 2)
      ]

-- | Built from Intruders in Chetwood, Eriador Wilds and Angmar Orcs, with Iârion from The
-- Lost Realm set, whom the stage's setup takes out again. One stage.
instance RunMessage IntrudersInChetwood where
  runMessage msg (IntrudersInChetwood attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.IntrudersInChetwood, EncounterSet.EriadorWilds, EncounterSet.AngmarOrcs, EncounterSet.TheLostRealm]
      putQuestIntoPlay =<< genEncounterCard Quests.stopTheWarParty
      pure $ IntrudersInChetwood $ attrs & encounterDeckL .~ encounterDeck
    _ -> IntrudersInChetwood <$> runMessage msg attrs
