module LordOfTheRings.Enemy.Cards.VeteranWainrider (veteranWainrider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (challengersAhead)

newtype VeteranWainrider = VeteranWainrider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

veteranWainrider :: EnemyCard VeteranWainrider
veteranWainrider = enemy VeteranWainrider Cards.veteranWainrider

-- | While The Challengers is ahead of The Wainriders, Veteran Wainrider gets +2 Threat and
-- cannot leave the staging area.
instance HasModifiersFor VeteranWainrider where
  getModifiersFor (VeteranWainrider a) = whenM challengersAhead $ modifySelf a [ThreatModifier 2, CannotLeaveStagingArea]

-- | Shadow: Attacking enemy gets +2 Attack if The Challengers is ahead of The Wainriders.
instance RunMessage VeteranWainrider where
  runMessage msg e@(VeteranWainrider attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      whenM challengersAhead $ enemyGetsAttack attrs eid 2
      pure e
    _ -> VeteranWainrider <$> runMessage msg attrs
