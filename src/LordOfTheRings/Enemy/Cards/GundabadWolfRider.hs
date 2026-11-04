module LordOfTheRings.Enemy.Cards.GundabadWolfRider (gundabadWolfRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (stagesWithoutProgress)

newtype GundabadWolfRider = GundabadWolfRider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gundabadWolfRider :: EnemyCard GundabadWolfRider
gundabadWolfRider = enemy GundabadWolfRider Cards.gundabadWolfRider

-- | Forced: When this enemy attacks, remove 1 progress from the current quest.
instance HasAbilities GundabadWolfRider where
  getAbilities (GundabadWolfRider a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Attacking enemy gets +1 attack for each quest stage in play with no progress on
-- it.
instance RunMessage GundabadWolfRider where
  runMessage msg e@(GundabadWolfRider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeProgressFromQuest attrs 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- stagesWithoutProgress
      when (n > 0) $ enemyGetsAttack (CardIdSource cid) eid n
      pure e
    _ -> GundabadWolfRider <$> runMessage msg attrs
