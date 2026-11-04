module LordOfTheRings.Enemy.Cards.HoundOfSauron (houndOfSauron) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype HoundOfSauron = HoundOfSauron EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

houndOfSauron :: EnemyCard HoundOfSauron
houndOfSauron = enemy HoundOfSauron Cards.houndOfSauron

-- | Hound of Sauron gets -5 engagement cost for each enemy in the staging area.
instance HasModifiersFor HoundOfSauron where
  getModifiersFor (HoundOfSauron a) = do
    staged <- length <$> select EnemyInStagingArea
    when (staged > 0) $ modifySelf a [EngagementCostModifier (-5 * staged)]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if it was engaged this round).
instance RunMessage HoundOfSauron where
  runMessage msg e@(HoundOfSauron attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      engaged <- elem EngagedThisRound <$> getModifiers eid
      enemyGetsAttack attrs eid (if engaged then 2 else 1)
      pure e
    _ -> HoundOfSauron <$> runMessage msg attrs
