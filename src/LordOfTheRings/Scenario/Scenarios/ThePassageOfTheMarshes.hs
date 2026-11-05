module LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes (thePassageOfTheMarshes) where

import LordOfTheRings.Ability (mkAbility, forced)
import LordOfTheRings.Matcher.Window (WindowMatcher (ExploredLocationWindow))
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Window (Window (Window), WindowType (Explored))
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (getRemainingPlayers, getMire, LocationMatcher (LocationInStagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (passageOfTheMarshesResolution)
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype ThePassageOfTheMarshes = ThePassageOfTheMarshes ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePassageOfTheMarshes :: Scenario
thePassageOfTheMarshes = Scenario $ scenario (ThePassageOfTheMarshes . (nightmareDeckL ?~ nightmare)) "the-passage-of-the-marshes" "The Passage of the Marshes"
 where
  nightmare = nightmareDeck Nightmare.thePassageOfTheMarshesNightmare EncounterSet.ThePassageOfTheMarshesNightmare
    [(Enemies.gropingDead, 3), (Enemies.deadThings, 3), (Locations.softMires, 3), (Locations.hauntedMere, 3), (Treacheries.bitterReek, 3)]

instance HasAbilities ThePassageOfTheMarshes where
  getAbilities (ThePassageOfTheMarshes a) =
    [mkAbility a 1 $ forced $ ExploredLocationWindow When LocationInStagingArea | a.nightmare]

{- | The Passage of the Marshes, Gollum and Morgul Nazgûl make up the encounter deck (The
Land of Shadow rules insert). Gollum is not gathered, having no encounter card back: stage
1A puts him into play.
-}
instance RunMessage ThePassageOfTheMarshes where
  runMessage msg (ThePassageOfTheMarshes attrs) = case msg of
    UseCardAbility _ ScenarioSource 1 windows | attrs.nightmare -> do
      for_ [lid | Window _ (Explored lid) <- windows] \lid -> do
        n <- getMire lid
        eachPlayer \pid -> encounterRaisesThreat pid n
      pure $ ThePassageOfTheMarshes attrs
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets attrs [EncounterSet.ThePassageOfTheMarshes, EncounterSet.GollumSaga, EncounterSet.NazgulSaga]
      firstStage <- genEncounterCard Quests.theTamingOfSmeagol
      rest <- traverse genEncounterCard [Quests.acrossTheMarshes, Quests.dontLookAtTheLights]
      putQuestIntoPlay firstStage
      pure $ ThePassageOfTheMarshes $ attrs & encounterDeckL .~ deck & questDeckL .~ rest
    -- Campaign: Setup: Each player shuffles 1 copy of The Searching Eye into his deck. (The
    -- burdens of Helm's Deep and The Road to Isengard are left out as the deck is dealt: see
    -- 'LordOfTheRings.Saga.removedBurdenSets'.)
    DoStep 9 Setup | isJust attrs.campaign -> do
      players <- getRemainingPlayers
      for_ players \pid -> shuffleIntoDeck pid =<< genEncounterCard Treacheries.theSearchingEye
      ThePassageOfTheMarshes <$> runMessage msg attrs
    ResolveCampaign | isJust attrs.campaign -> do
      passageOfTheMarshesResolution
      ThePassageOfTheMarshes <$> runMessage msg attrs
    _ -> ThePassageOfTheMarshes <$> runMessage msg attrs
