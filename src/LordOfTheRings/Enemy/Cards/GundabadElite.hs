module LordOfTheRings.Enemy.Cards.GundabadElite (gundabadElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (currentProgress)

newtype GundabadElite = GundabadElite EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

gundabadElite :: EnemyCard GundabadElite
gundabadElite = enemy GundabadElite Cards.gundabadElite

instance HasModifiersFor GundabadElite where
  getModifiersFor (GundabadElite a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]

instance HasAbilities GundabadElite where
  getAbilities (GundabadElite a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == StagingArea]

instance RunMessage GundabadElite where
  runMessage msg e@(GundabadElite a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      progress <- currentProgress
      replacement <- elem (QuestProgressRemovalBecomes ScenarioTarget) <$> getModifiers GameTarget
      chooseOrRunOneM lead do
        questionLabeled "gundabadElite.choose"
        causedBy a
        when (progress >= 2 || replacement) $ labeled "gundabadElite.remove" $ removeProgressFromQuest a 2
        labeled "gundabadElite.attack" $ immediateAttack lead a.id
      pure e
    _ -> GundabadElite <$> runMessage msg a
