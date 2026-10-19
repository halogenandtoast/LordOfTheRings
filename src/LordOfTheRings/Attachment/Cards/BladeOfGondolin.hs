module LordOfTheRings.Attachment.Cards.BladeOfGondolin (bladeOfGondolin) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype BladeOfGondolin = BladeOfGondolin AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

bladeOfGondolin :: AttachmentCard BladeOfGondolin
bladeOfGondolin = attachment BladeOfGondolin Cards.bladeOfGondolin

-- | Attached hero gets +1 attack when attacking an Orc.
instance HasModifiersFor BladeOfGondolin where
  getModifiersFor (BladeOfGondolin a) = case a.placement of
    AttachedTo host -> modified a host [AttackBonusAgainst Orc 1]
    _ -> pure ()

-- | Response: After attached hero attacks and destroys an enemy, place 1 progress token on
-- the current quest.
instance HasAbilities BladeOfGondolin where
  getAbilities (BladeOfGondolin a) =
    [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage BladeOfGondolin where
  runMessage msg s@(BladeOfGondolin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgressOnCurrentQuest a 1
      pure s
    _ -> BladeOfGondolin <$> runMessage msg a
