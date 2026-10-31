module LordOfTheRings.Attachment.Cards.RaimentOfTheSecondAge (raimentOfTheSecondAge) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype RaimentOfTheSecondAge = RaimentOfTheSecondAge AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from The Battle of Carn Dûm. Attach to a
-- character. Restricted.
raimentOfTheSecondAge :: AttachmentCard RaimentOfTheSecondAge
raimentOfTheSecondAge = attachment RaimentOfTheSecondAge Cards.raimentOfTheSecondAge & attachTo AttachToCharacter

-- | Attached character gets +2 attack and +2 hit points.
instance HasModifiersFor RaimentOfTheSecondAge where
  getModifiersFor (RaimentOfTheSecondAge a) = case a.placement of
    AttachedTo host -> modified a host [AttackModifier 2, HitPointModifier 2]
    _ -> pure ()

-- | Response: After attached character attacks and destroys an enemy, draw 1 card.
instance HasAbilities RaimentOfTheSecondAge where
  getAbilities (RaimentOfTheSecondAge a) =
    [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage RaimentOfTheSecondAge where
  runMessage msg s@(RaimentOfTheSecondAge a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> RaimentOfTheSecondAge <$> runMessage msg a
