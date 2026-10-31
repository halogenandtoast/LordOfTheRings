module LordOfTheRings.Enemy.Cards.FaithlessConspirator (faithlessConspirator) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (underneathCount)

newtype FaithlessConspirator = FaithlessConspirator EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

faithlessConspirator :: EnemyCard FaithlessConspirator
faithlessConspirator = enemy FaithlessConspirator Cards.faithlessConspirator

-- | If the active location has at least 1 facedown card underneath it, Faithless
-- Conspirator cannot take damage.
instance HasModifiersFor FaithlessConspirator where
  getModifiersFor (FaithlessConspirator a) = do
    under <- sum <$> (traverse underneathCount =<< select IsActiveLocation)
    when (under > 0) $ modified a a [CannotTakeDamage]

-- | Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage FaithlessConspirator where
  runMessage msg e@(FaithlessConspirator attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure e
    _ -> FaithlessConspirator <$> runMessage msg attrs
