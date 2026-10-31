module LordOfTheRings.Scenario.Scenarios.FlightFromMoria (flightFromMoria) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getRemainingPlayers)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype FlightFromMoria = FlightFromMoria ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

flightFromMoria :: Scenario
flightFromMoria = Scenario $ scenario (FlightFromMoria . (nightmareDeckL ?~ nightmare)) "flight-from-moria" "Flight from Moria"
 where
  -- its setup card's text is Search for an Exit's to keep: a bypassed stage is shuffled
  -- back, and Escape from Darkness limits Abandoned Tools
  nightmare =
    nightmareDeck
      Nightmare.flightFromMoriaNightmare
      EncounterSet.FlightFromMoriaNightmare
      [ (Enemies.strayGoblin, 3)
      , (Treacheries.chanceEncounter, 3)
      , (Locations.theMountainsRoots, 2)
      , (Treacheries.newDevilry, 1)
      , (Treacheries.caveIn, 3)
      , (Treacheries.darkAndDreadful, 2)
      , (Enemies.goblinSpearman, 1)
      , (Enemies.goblinArcher, 2)
      , (Locations.plunderedArmoury, 2)
      ]

instance RunMessage FlightFromMoria where
  runMessage msg (FlightFromMoria attrs) = case msg of
    -- A Presence in the Dark's setup keeps 1 copy of A Foe Beyond per player in the deck;
    -- the Search for an Exit stages are shuffled into the quest deck beneath it, the
    -- nightmare set's 3 among them in nightmare mode
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.FlightFromMoria, EncounterSet.DeepsOfMoria, EncounterSet.HazardsOfThePit, EncounterSet.PlunderingGoblins]
      players <- length <$> getRemainingPlayers
      let (foes, rest) = partition ((== toCardCode Treacheries.aFoeBeyond) . toCardCode) cards
      firstStage <- genEncounterCard Quests.aPresenceInTheDark
      laterStages <-
        shuffleM
          =<< traverse
            genEncounterCard
            ( [ Quests.headingDown
              , Quests.headingUp
              , Quests.aWrongTurn
              , Quests.narrowPaths
              , Quests.hastyCouncil
              , Quests.blockedByShadow
              , Quests.escapeFromDarkness
              ]
                <> (if attrs.nightmare then [Quests.pursuedByShadowStage, Quests.blockedByFlame, Quests.ghash] else [])
            )
      putQuestIntoPlay firstStage
      pure $ FlightFromMoria $ attrs & encounterDeckL .~ take players foes <> rest & questDeckL .~ laterStages
    -- a Search for an Exit stage comes out on side A, and flips itself at the staging step
    AdvanceToQuest card -> do
      putQuestIntoPlay card
      pure $ FlightFromMoria attrs
    _ -> FlightFromMoria <$> runMessage msg attrs
