module LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies (theBattleOfFiveArmies) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLead, getMainQuests)
import LordOfTheRings.Modifier (ModifierType (ProgressLimit, QuestProgressRemovalBecomes))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (chosenStage)
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Token (Token (Progress))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (RoundBegins))

newtype TheBattleOfFiveArmies = TheBattleOfFiveArmies ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfFiveArmies :: Scenario
theBattleOfFiveArmies =
  Scenario
    $ scenario
      (TheBattleOfFiveArmies . (nightmareDeckL ?~ nightmare))
      "the-battle-of-five-armies"
      "The Battle of Five Armies"
 where
  -- https://hallofbeorn.com/LotR?EncounterSet=The+Battle+of+Five+Armies+Nightmare
  nightmare =
    nightmareDeck
      Nightmare.theBattleOfFiveArmiesNightmare
      EncounterSet.TheBattleOfFiveArmiesNightmare
      [ (Enemies.darkBats, 3)
      , (Locations.hiddenPath, 3)
      , (Treacheries.fightingAmongFriends, 2)
      , (Enemies.vanguardOfBolg, 1)
      , (Enemies.ravenousWarg, 1)
      , (Enemies.gundabadClimber, 4)
      , (Enemies.gundabadWolfRider, 3)
      , (Objectives.mistyMountainEagle, 1)
      ]

{- | Progress cannot be placed on any quest stage except for the current quest stage (The
Hobbit: On the Doorstep rules insert, "Current Quest Stage").
-}
instance HasModifiersFor TheBattleOfFiveArmies where
  getModifiersFor (TheBattleOfFiveArmies a) = do
    when a.nightmare do
      stage5 <-
        getsGame
          ( any (\q -> toCardCode q == toCardCode Quests.theCloudsBurst && q.flipped)
              . toList
              . (.entities.quests)
          )
      when stage5 $ modified a GameTarget [QuestProgressRemovalBecomes ScenarioTarget]
    mains <- getMainQuests
    when (length mains > 1) do
      chosen <- chosenStage
      for_ mains \q -> when (Just q /= chosen) $ modified a q [ProgressLimit 0]

{- | The Battle of Five Armies and Wilderland make up the encounter deck (The Hobbit: On the
Doorstep rules insert). Stage 1 sets up the rest. The first player takes control of Bilbo
Baggins as the game sets up (see 'LordOfTheRings.Saga.fellowshipSetup').

At the beginning of each round, before players have collected resources, the first player
must decide which stage will be the current quest stage for that round (rules insert,
"Current Quest Stage"): asked as the round begins, while stages 2 to 4 are in play.

Nothing is discovered here: its cards name no treasure.
-}
instance RunMessage TheBattleOfFiveArmies where
  runMessage msg (TheBattleOfFiveArmies attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheBattleOfFiveArmies, EncounterSet.Wilderland]
      firstStage <- genEncounterCard Quests.theGatheringOfTheClouds
      laterStages <-
        traverse
          genEncounterCard
          [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill, Quests.theCloudsBurst]
      putQuestIntoPlay firstStage
      pure $ TheBattleOfFiveArmies $ attrs & encounterDeckL .~ cards & questDeckL .~ laterStages
    PlaceTokens _ ScenarioTarget Progress n | attrs.nightmare && n > 0 -> do
      let total = nightmareProgress attrs + n
      players <- getsGame (length . (.playerOrder))
      when (total >= 2 * players + 3) $ gameOver Lost
      pure $ TheBattleOfFiveArmies $ attrs & metaL .~ object ["nightmareProgress" .= total]
    CheckWindows ws | Window When RoundBegins `elem` ws -> do
      mains <- getMainQuests
      when (length mains > 1) do
        lead <- getLead
        chooseOneM lead do
          questionLabeled "theBattleOfFiveArmies.currentQuest"
          targets mains setCurrentQuest
      TheBattleOfFiveArmies <$> runMessage msg attrs
    _ -> TheBattleOfFiveArmies <$> runMessage msg attrs

nightmareProgress :: ScenarioAttrs -> Int
nightmareProgress attrs =
  fromMaybe 0
    $ parseMaybe
      (withObject "Nightmare progress" (\o -> o .:? "nightmareProgress" .!= 0))
      attrs.scenarioMeta
