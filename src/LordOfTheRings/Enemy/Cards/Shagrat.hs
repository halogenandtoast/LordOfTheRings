module LordOfTheRings.Enemy.Cards.Shagrat (shagrat) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Boon))

newtype Shagrat = Shagrat EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

shagrat :: EnemyCard Shagrat
shagrat = enemy Shagrat Cards.shagrat

-- | Cannot have non-boon attachments. Cannot take non-combat damage. Allies cannot attack or
-- defend against Shagrat.
instance HasModifiersFor Shagrat where
  getModifiersFor (Shagrat a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Boon, CannotTakeNonCombatDamage, NotFoughtByAllies]

-- | Forced: After progress is placed on the quest, Shagrat gets -X engagement cost until
-- the end of the round. X is the amount of progress just placed. "The quest" is the current
-- quest: progress on a side quest the players are not questing against does not count (The
-- Lost Realm rules insert, "Side Quests").
instance HasAbilities Shagrat where
  getAbilities (Shagrat a) = [mkAbility a 1 $ forced $ ProgressPlacedOnQuestWindow After]

instance RunMessage Shagrat where
  runMessage msg e@(Shagrat attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      let x = sum [n | Window _ (ProgressPlacedOnCurrentQuest n) <- ws]
      when (x > 0) $ roundModifier attrs attrs.id (EngagementCostModifier (negate x))
      pure e
    _ -> Shagrat <$> runMessage msg attrs
