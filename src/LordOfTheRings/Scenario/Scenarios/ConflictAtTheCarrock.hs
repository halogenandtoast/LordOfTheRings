module LordOfTheRings.Scenario.Scenarios.ConflictAtTheCarrock (conflictAtTheCarrock) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype ConflictAtTheCarrock = ConflictAtTheCarrock ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

conflictAtTheCarrock :: Scenario
conflictAtTheCarrock = Scenario $ scenario ConflictAtTheCarrock "conflict-at-the-carrock" "Conflict at the Carrock"

instance RunMessage ConflictAtTheCarrock where
  runMessage msg (ConflictAtTheCarrock attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.ConflictAtTheCarrock, EncounterSet.JourneyAlongTheAnduin, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.grimbeornsQuest
      laterStages <- traverse genEncounterCard [Quests.againstTheTrolls]
      putQuestIntoPlay firstStage
      pure $ ConflictAtTheCarrock $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Louis, Morris, Stuart and Rupert: Response: After defeating this enemy, you may
    -- choose and discard 1 "Sacked!" card from play. Asked as the Troll goes, while it can
    -- still be told apart.
    Destroy (EnemyTarget eid) -> do
      present <- selectAny (EnemyWithId eid)
      troll <- if present then (`elem` uniqueTrolls) . toCardCode <$> getEnemy eid else pure False
      sacked <- getsGame \g -> [t | t <- toList g.entities.treacheries, toCardCode t == toCardCode Treacheries.sacked]
      lead <- getLead
      when (troll && notNull sacked) $ chooseOneM lead do
        questionLabeled "conflictAtTheCarrock.discardSacked"
        targets sacked discard
        labeled "pass" $ pure ()
      ConflictAtTheCarrock <$> runMessage msg attrs
    _ -> ConflictAtTheCarrock <$> runMessage msg attrs
   where
    uniqueTrolls = map toCardCode [Enemies.louis, Enemies.morris, Enemies.stuart, Enemies.rupert]
