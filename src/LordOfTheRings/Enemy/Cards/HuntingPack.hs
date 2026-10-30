module LordOfTheRings.Enemy.Cards.HuntingPack (huntingPack) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype HuntingPack = HuntingPack EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

huntingPack :: EnemyCard HuntingPack
huntingPack = enemy HuntingPack Cards.huntingPack

-- | Cannot have attachments.
-- When a player is making an engagement check against Hunting Pack, it gets -1
-- engagement cost for each damaged character that player controls.
instance HasModifiersFor HuntingPack where
  getModifiersFor (HuntingPack a) = do
    players <- getRemainingPlayers
    against <- for players \pid -> do
      damaged <- length <$> (filterM (fmap (> 0) . getDamage) =<< charactersControlledBy pid)
      pure [EngagementCostAgainst pid (negate damaged) | damaged > 0]
    modified a a $ CannotHaveAttachments : concat against

instance RunMessage HuntingPack where
  runMessage msg (HuntingPack attrs) = HuntingPack <$> runMessage msg attrs
