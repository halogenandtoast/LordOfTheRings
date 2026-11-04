module LordOfTheRings.Enemy.Cards.SnowTroll (snowTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SnowTroll = SnowTroll EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowTroll :: EnemyCard SnowTroll
snowTroll = enemy SnowTroll Cards.snowTroll

-- | Cannot have player card attachments.
instance HasModifiersFor SnowTroll where
  getModifiersFor (SnowTroll a) = modifySelf a [CannotHavePlayerAttachments]

-- | Forced: After Snow-troll attacks and destroys a character, return it to the staging
-- area.
instance HasAbilities SnowTroll where
  getAbilities (SnowTroll a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage SnowTroll where
  runMessage msg e@(SnowTroll attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      returnToStagingAfterAttack attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> SnowTroll <$> runMessage msg attrs
