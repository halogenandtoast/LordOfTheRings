module LordOfTheRings.Attachment.Cards.Resolute (resolute) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Resolute = Resolute AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Mysterious Omens. Permanent.
resolute :: AttachmentCard Resolute
resolute = attachment Resolute Cards.resolute

-- | Attached hero gets +1 willpower.
instance HasModifiersFor Resolute where
  getModifiersFor (Resolute a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 1]
    _ -> pure ()

-- | Response: Cancel up to 2 points of damage just dealt to attached hero. (Limit once per
-- game.) As Raven-winged Helm, the damage just placed is taken off again.
instance HasAbilities Resolute where
  getAbilities (Resolute a) =
    [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ response (dealtDamage host) | AttachedTo host <- [a.placement]]

instance RunMessage Resolute where
  runMessage msg s@(Resolute a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      let dealt = sum [n | Window _ (DealtDamage _ n) <- windows]
      withHost a \host -> removeTokens a host Damage (min 2 dealt)
      pure s
    _ -> Resolute <$> runMessage msg a
