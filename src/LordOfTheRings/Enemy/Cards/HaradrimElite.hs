module LordOfTheRings.Enemy.Cards.HaradrimElite (haradrimElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype HaradrimElite = HaradrimElite EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimElite :: EnemyCard HaradrimElite
haradrimElite = enemy HaradrimElite Cards.haradrimElite

-- | Forced: When Haradrim Elite enters play, it makes an immediate attack from the staging
-- area against the first player.
-- Shadow: This enemy attacks again after this attack resolves. (Deal a new shadow card for
-- that attack.)
instance RunMessage HaradrimElite where
  runMessage msg e@(HaradrimElite attrs) = case msg of
    -- ponytail: entering play into the staging area is read off its ambush check, which
    -- every enemy revealed or added to the staging area makes, and none returning there
    AmbushEngagementCheck eid | eid == attrs.id -> do
      lead <- getLead
      immediateAttack lead attrs.id
      HaradrimElite <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      afterAttack eid [DealShadowCard eid, EnemyAttack pid eid]
      pure e
    _ -> HaradrimElite <$> runMessage msg attrs
