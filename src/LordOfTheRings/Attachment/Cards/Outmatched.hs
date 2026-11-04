module LordOfTheRings.Attachment.Cards.Outmatched (outmatched) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Outmatched = Outmatched AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play into your play area unattached. If unattached, attach to the next eligible enemy
-- that engages you, as Followed does.
outmatched :: AttachmentCard Outmatched
outmatched = attachment Outmatched Cards.outmatched & attachTo PlayIntoPlayArea

-- | Response: After attached enemy resolves its attack, ready the defending character.
instance HasAbilities Outmatched where
  getAbilities (Outmatched a) =
    [mkAbility a 1 $ response $ DefendedAttackAgainstWindow After eid | AttachedTo (EnemyTarget eid) <- [a.placement]]

instance RunMessage Outmatched where
  runMessage msg s@(Outmatched a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [t | Window _ (DefendedAttack t _) <- windows] ready
      pure s
    _ -> Outmatched <$> runMessage msg a
