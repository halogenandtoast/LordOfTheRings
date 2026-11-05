module LordOfTheRings.Enemy.Cards.GundabadStalker (gundabadStalker) where

import LordOfTheRings.Card (cdUnique, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GundabadStalker = GundabadStalker EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, shuffled in by The Ghost of Framsburg's campaign
-- card.
gundabadStalker :: EnemyCard GundabadStalker
gundabadStalker = enemy GundabadStalker Cards.gundabadStalker

-- | Gundabad Stalker cannot be optionally engaged except by a player who is engaged with a
-- unique enemy.
instance HasModifiersFor GundabadStalker where
  getModifiersFor (GundabadStalker a) = modifySelf a [CannotBeOptionallyEngagedBy (NotPlayer $ PlayerEngagedWith UniqueEnemy)]

-- | Shadow: If attacking enemy is unique, return it to the staging area after this attack.
instance RunMessage GundabadStalker where
  runMessage msg e@(GundabadStalker attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      e' <- getEnemy eid
      when (cdUnique (toCardDef e')) $ returnToStagingAfterAttack eid
      pure e
    _ -> GundabadStalker <$> runMessage msg attrs
