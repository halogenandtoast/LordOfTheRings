module LordOfTheRings.Enemy.Cards.Crebain (crebain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype Crebain = Crebain EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

crebain :: EnemyCard Crebain
crebain = enemy Crebain Cards.crebain

-- | Surge.
-- While Crebain is in the staging area, encounter card effects cannot be canceled.
instance HasModifiersFor Crebain where
  getModifiersFor (Crebain a) = when (a.placement == StagingArea) $ modified a GameTarget [CannotCancelEncounterEffects]

-- | Shadow: Return attacking enemy to the staging area after it attacks.
instance RunMessage Crebain where
  runMessage msg e@(Crebain attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure e
    _ -> Crebain <$> runMessage msg attrs
