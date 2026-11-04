module LordOfTheRings.Attachment.Cards.MailOfEarnur (mailOfEarnur) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MailOfEarnur = MailOfEarnur AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign. Attach to a hero. Restricted.
mailOfEarnur :: AttachmentCard MailOfEarnur
mailOfEarnur = attachment MailOfEarnur Cards.mailOfEarnur

-- | Attached hero gets +2 hit points.
instance HasModifiersFor MailOfEarnur where
  getModifiersFor (MailOfEarnur a) = case a.placement of
    AttachedTo host -> modified a host [HitPointModifier 2]
    _ -> pure ()

-- | Response: After a player's threat is reduced, heal 1 damage from attached hero.
instance HasAbilities MailOfEarnur where
  getAbilities (MailOfEarnur a) = [mkAbility a 1 $ response $ ThreatReducedWindow After | AttachedTo _ <- [a.placement]]

instance RunMessage MailOfEarnur where
  runMessage msg s@(MailOfEarnur a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> healDamage a host 1
      pure s
    _ -> MailOfEarnur <$> runMessage msg a
