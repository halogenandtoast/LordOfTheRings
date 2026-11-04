module LordOfTheRings.Enemy.Cards.KingsKnight (kingsKnight) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype KingsKnight = KingsKnight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

kingsKnight :: EnemyCard KingsKnight
kingsKnight = enemy KingsKnight Cards.kingsKnight

-- | Forced: After King's Knight attacks, if no damage was dealt by its attack, return it to
-- the staging area.
instance HasAbilities KingsKnight where
  getAbilities (KingsKnight a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Shadow: If no damage is dealt by this attack, return attacking enemy to the staging area
-- after this attack. Both read the damage the attack dealt once it is over.
instance RunMessage KingsKnight where
  runMessage msg e@(KingsKnight attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ IfAttackDealtNoDamage attrs.id [PlaceEnemy attrs.id StagingArea]
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      afterAttack eid [IfAttackDealtNoDamage eid [PlaceEnemy eid StagingArea]]
      pure e
    _ -> KingsKnight <$> runMessage msg attrs
