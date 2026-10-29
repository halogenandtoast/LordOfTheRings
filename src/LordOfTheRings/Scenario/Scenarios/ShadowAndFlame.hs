module LordOfTheRings.Scenario.Scenarios.ShadowAndFlame (shadowAndFlame) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Modifier (ModifierType (HeroCommittedThisRound))
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (GameSource))

newtype ShadowAndFlame = ShadowAndFlame ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowAndFlame :: Scenario
shadowAndFlame = Scenario $ scenario ShadowAndFlame "shadow-and-flame" "Shadow and Flame"

instance RunMessage ShadowAndFlame where
  runMessage msg (ShadowAndFlame attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.ShadowAndFlame, EncounterSet.DeepsOfMoria, EncounterSet.GoblinsOfTheDeep]
      firstStage <- genEncounterCard Quests.nearingTheGate
      laterStages <- traverse genEncounterCard [Quests.theRearGuard, Quests.lastLordOfMoria]
      putQuestIntoPlay firstStage
      pure $ ShadowAndFlame $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- a hero committed this round, as The Rear Guard reads, even one since removed
    CommitToQuest (HeroTarget _) -> do
      roundModifier GameSource GameTarget HeroCommittedThisRound
      ShadowAndFlame <$> runMessage msg attrs
    _ -> ShadowAndFlame <$> runMessage msg attrs
