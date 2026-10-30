module LordOfTheRings.Quest.Cards.LieLow (lieLow) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (isBlank)

newtype LieLow = LieLow QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
lieLow :: QuestCard LieLow
lieLow = quest LieLow Cards.lieLow

-- | Forced: At the end of the quest phase, if no progress was placed on Lie Low this round,
-- each enemy gets -20 engagement cost until the end of the round.
instance HasAbilities LieLow where
  getAbilities (LieLow a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase]

-- | Response: After this stage is defeated, choose an enemy in the staging area and shuffle
-- it into the encounter deck. The players gain by it, so it is taken.
instance RunMessage LieLow where
  runMessage msg q@(LieLow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placed <- progressPlacedThisRound (toTarget attrs)
      when (placed == 0) do
        enemies <- select AnyEnemy
        for_ enemies \e -> roundModifier attrs e (EngagementCostModifier (-20))
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      unlessM (isBlank attrs) do
        lead <- getLead
        enemies <- select EnemyInStagingArea
        chooseOneM lead do
          questionLabeled "lieLow.shuffle"
          causedBy attrs
          targets enemies shuffleIntoEncounterDeck
      LieLow <$> runMessage msg attrs
    _ -> LieLow <$> runMessage msg attrs
