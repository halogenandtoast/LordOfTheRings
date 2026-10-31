module LordOfTheRings.Scenario.Scenarios.TheRedhornGate (theRedhornGate) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheRedhornGate = TheRedhornGate ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Nightmare: "Setup: Remove all copies of Blinding Blizzard from the encounter deck and
set them aside, out of play. During this game, player cards in the victory display do not
count their victory points. Forced: When stage 2b is revealed, shuffle each set aside copy
of Blinding Blizzard into the encounter deck." The Mountains' Peaks counts the points, and
Snowdrifts shuffles the Blizzards in.
-}
theRedhornGate :: Scenario
theRedhornGate =
  Scenario
    $ scenario (TheRedhornGate . (nightmareDeckL ?~ nightmare)) "the-redhorn-gate" "The Redhorn Gate"
 where
  nightmare =
    nightmareDeck
      Nightmare.theRedhornGateNightmare
      EncounterSet.TheRedhornGateNightmare
      [ (Enemies.blackUruks, 1)
      , (Enemies.mountainWarg, 3)
      , (Locations.kneesOfTheMountain, 1)
      , (Locations.turbulentWaters, 2)
      , (Locations.wargLair, 2)
      , (Locations.theDimrillStair, 1)
      , (Treacheries.fellVoices, 2)
      , (Treacheries.fallenStones, 2)
      , (Enemies.mountainGoblin, 3)
      ]

instance RunMessage TheRedhornGate where
  runMessage msg (TheRedhornGate attrs) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets attrs [EncounterSet.TheRedhornGate, EncounterSet.MistyMountains]
      let (blizzards, encounterDeck) = partition (cardMatches $ cardIs Treacheries.blindingBlizzard) deck
      firstStage <- genEncounterCard Quests.upThePass
      laterStages <- traverse genEncounterCard [Quests.snowdrifts, Quests.theMountainsPeaks]
      putQuestIntoPlay firstStage
      pure
        $ TheRedhornGate
        $ attrs
        & (encounterDeckL .~ encounterDeck)
        & (setAsideL %~ (<> blizzards))
        & (questDeckL .~ laterStages)
    _ -> TheRedhornGate <$> runMessage msg attrs
