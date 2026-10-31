module LordOfTheRings.Scenario.Scenarios.IntoIthilien (intoIthilien) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Entities (entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype IntoIthilien = IntoIthilien ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoIthilien :: Scenario
intoIthilien =
  Scenario
    $ scenario (IntoIthilien . (nightmareDeckL ?~ nightmare)) "into-ithilien" "Into Ithilien"
 where
  nightmare =
    nightmareDeck
      Nightmare.intoIthilienNightmare
      EncounterSet.IntoIthilienNightmare
      [ (Locations.overgrownTrail, 1)
      , (Locations.secludedGlade, 3)
      , (Enemies.forestBat, 3)
      , (Treacheries.watcherInTheWoods, 2)
      , (Treacheries.blockingWargs, 1)
      , (Enemies.southronMercenaries, 3)
      ]

isMumakElite :: EncounterCard -> Bool
isMumakElite = (== toCardCode Enemies.mumakElite) . toCardCode

instance RunMessage IntoIthilien where
  runMessage msg (IntoIthilien attrs) = case msg of
    DoStep 1 Setup -> do
      gathered <-
        gatherEncounterSets
          attrs
          [EncounterSet.IntoIthilien, EncounterSet.BroodingForest, EncounterSet.CreaturesOfTheForest, EncounterSet.Southrons]
      -- Nightmare: "Setup: Set both copies of Mumak Elite aside, out of play."
      let (mumaks, encounterDeck) = partition isMumakElite gathered
      firstStage <- genEncounterCard Quests.ambushInIthilien
      laterStages <- traverse genEncounterCard [Quests.southronCounterAttack, Quests.theHiddenWay, Quests.approachingCairAndros]
      putQuestIntoPlay firstStage
      pure
        $ IntoIthilien
        $ attrs
        & (encounterDeckL .~ encounterDeck)
        & (questDeckL .~ laterStages)
        & (setAsideL %~ (<> mumaks))
    -- Nightmare: "Forced: When quest stage 2A or 4A are revealed, add 1 of the previously
    -- set aside copies of Mumak Elite to the staging area."
    PutQuestIntoPlay card
      | attrs.nightmare
      , toCardCode card `elem` map toCardCode [Quests.southronCounterAttack, Quests.approachingCairAndros]
      , Just mumak <- find isMumakElite attrs.scenarioSetAside -> do
          addToStaging mumak
          IntoIthilien <$> runMessage msg (attrs & setAsideL %~ filter (/= mumak))
    -- the stage bypassed leaves the quest deck before the next is revealed: stage 2 when
    -- the players complete stage 1 with Celador in the staging area, and stage 3 always
    -- after stage 2
    CompleteQuest qid -> do
      completed <- getsGame (fmap toCardCode . Map.lookup qid . entitiesQuests . gameEntities)
      celador <-
        getsGame
          ( any (\o -> toCardCode o == toCardCode Objectives.celador && o.placement == StagingArea)
              . entitiesObjectives
              . gameEntities
          )
      let bypassed
            | completed == Just (toCardCode Quests.ambushInIthilien) && celador = [Quests.southronCounterAttack]
            | completed == Just (toCardCode Quests.southronCounterAttack) = [Quests.theHiddenWay]
            | otherwise = []
          kept = filter (\c -> toCardCode c `notElem` map toCardCode bypassed) attrs.questDeck
      IntoIthilien <$> runMessage msg (attrs & questDeckL .~ kept)
    _ -> IntoIthilien <$> runMessage msg attrs
