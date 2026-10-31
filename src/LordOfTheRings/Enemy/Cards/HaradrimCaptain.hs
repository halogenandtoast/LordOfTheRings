module LordOfTheRings.Enemy.Cards.HaradrimCaptain (haradrimCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Trait (Trait (Harad))

newtype HaradrimCaptain = HaradrimCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimCaptain :: EnemyCard HaradrimCaptain
haradrimCaptain = enemy HaradrimCaptain Cards.haradrimCaptain

-- | Each Harad enemy in the staging area gains archery 1.
-- Each engaged Harad enemy gets +1 defense.
instance HasModifiersFor HaradrimCaptain where
  getModifiersFor (HaradrimCaptain a) = do
    staged <- select (EnemyWithTrait Harad <> EnemyInStagingArea)
    for_ staged \e -> modified a e [GainKeyword (Archery 1)]
    engaged <- select (EnemyWithTrait Harad <> EnemyEngagedWith AnyPlayer)
    for_ engaged \e -> modified a e [DefenseModifier 1]

-- | Shadow: Attaching enemy gets +1 attack (+3 attack instead if it has the Harad trait).
instance RunMessage HaradrimCaptain where
  runMessage msg e@(HaradrimCaptain attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      harad <- selectAny (EnemyWithId eid <> EnemyWithTrait Harad)
      enemyGetsAttack attrs eid (if harad then 3 else 1)
      pure e
    _ -> HaradrimCaptain <$> runMessage msg attrs
