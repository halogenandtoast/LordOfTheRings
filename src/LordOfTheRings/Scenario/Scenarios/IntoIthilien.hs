module LordOfTheRings.Scenario.Scenarios.IntoIthilien (intoIthilien) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Entities (entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype IntoIthilien = IntoIthilien ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoIthilien :: Scenario
intoIthilien = Scenario $ scenario IntoIthilien "into-ithilien" "Into Ithilien"

instance RunMessage IntoIthilien where
  runMessage msg (IntoIthilien attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.IntoIthilien, EncounterSet.BroodingForest, EncounterSet.CreaturesOfTheForest, EncounterSet.Southrons]
      firstStage <- genEncounterCard Quests.ambushInIthilien
      laterStages <- traverse genEncounterCard [Quests.southronCounterAttack, Quests.theHiddenWay, Quests.approachingCairAndros]
      putQuestIntoPlay firstStage
      pure $ IntoIthilien $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
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
