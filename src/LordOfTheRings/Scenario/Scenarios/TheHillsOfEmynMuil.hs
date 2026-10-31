module LordOfTheRings.Scenario.Scenarios.TheHillsOfEmynMuil (theHillsOfEmynMuil) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (LocationMatcher (LocationInStagingArea), select)
import LordOfTheRings.Modifier (ModifierType (ProgressPerRoundLimit))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheHillsOfEmynMuil = TheHillsOfEmynMuil ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHillsOfEmynMuil :: Scenario
theHillsOfEmynMuil =
  Scenario
    $ scenario (TheHillsOfEmynMuil . (nightmareDeckL ?~ nightmare)) "the-hills-of-emyn-muil" "The Hills of Emyn Muil"
 where
  nightmare =
    nightmareDeck
      Nightmare.theHillsOfEmynMuilNightmare
      EncounterSet.TheHillsOfEmynMuilNightmare
      [ (Enemies.dolGuldurOrcs, 2)
      , (Enemies.chieftanUfthak, 1)
      , (Enemies.dolGuldurBeastmaster, 2)
      , (Treacheries.theNecromancersReach, 2)
      , (Locations.necromancersPass, 2)
      , (Locations.enchantedStream, 2)
      , (Treacheries.evilStorm, 3)
      , (Treacheries.pursuedByShadow, 2)
      , (Locations.theEastWallOfRohan, 1)
      , (Locations.theNorthStair, 1)
      , (Locations.theShoresOfNenHithoel, 2)
      , (Locations.theHighlands, 4)
      , (Treacheries.impassableChasm, 2)
      , (Treacheries.slickFooting, 2)
      ]

-- | Nightmare: "No more than 1 progress may be placed on each location in the staging area
-- each round."
instance HasModifiersFor TheHillsOfEmynMuil where
  getModifiersFor (TheHillsOfEmynMuil a) = when a.nightmare do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressPerRoundLimit 1]

instance RunMessage TheHillsOfEmynMuil where
  runMessage msg (TheHillsOfEmynMuil attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.TheHillsOfEmynMuil, EncounterSet.DolGuldurOrcs, EncounterSet.SauronsReach]
      putQuestIntoPlay =<< genEncounterCard Quests.theHillsOfEmynMuil
      pure $ TheHillsOfEmynMuil $ attrs & encounterDeckL .~ encounterDeck
    _ -> TheHillsOfEmynMuil <$> runMessage msg attrs
