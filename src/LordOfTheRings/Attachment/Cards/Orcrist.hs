module LordOfTheRings.Attachment.Cards.Orcrist (orcrist) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Orcrist = Orcrist AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy or location), on its card def. Attach to a hero. Restricted.
orcrist :: AttachmentCard Orcrist
orcrist = attachment Orcrist Cards.orcrist

-- | Attached hero gets +2 [attack].
instance HasModifiersFor Orcrist where
  getModifiersFor (Orcrist a) = withHost a \host -> modified a host [AttackModifier 2]

-- | Response: After attached hero destroys an enemy, add 1 resource to attached hero's
-- pool.
instance HasAbilities Orcrist where
  getAbilities (Orcrist a) =
    [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage Orcrist where
  runMessage msg s@(Orcrist a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> Orcrist <$> runMessage msg a
