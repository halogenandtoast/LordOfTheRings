module LordOfTheRings.Enemy.Cards.SoldierOfNurn (soldierOfNurn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (placeFromHandUnderCastleSideQuest)

newtype SoldierOfNurn = SoldierOfNurn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

soldierOfNurn :: EnemyCard SoldierOfNurn
soldierOfNurn = enemy SoldierOfNurn Cards.soldierOfNurn

-- | Forced: After Soldier of Nurn attacks, place 1 card from your hand facedown under a
-- Castle side quest.
instance HasAbilities SoldierOfNurn where
  getAbilities (SoldierOfNurn a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, place 1
-- card from your hand facedown under a Castle side quest.
instance RunMessage SoldierOfNurn where
  runMessage msg e@(SoldierOfNurn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] (placeFromHandUnderCastleSideQuest attrs)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      placeFromHandUnderCastleSideQuest attrs pid
      pure e
    _ -> SoldierOfNurn <$> runMessage msg attrs
