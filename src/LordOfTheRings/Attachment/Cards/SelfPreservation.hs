module LordOfTheRings.Attachment.Cards.SelfPreservation (selfPreservation) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype SelfPreservation = SelfPreservation AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

selfPreservation :: AttachmentCard SelfPreservation
selfPreservation = attachment SelfPreservation Cards.selfPreservation & attachTo AttachToCharacter

-- | Action: Exhaust Self Preservation to heal 2 points of damage from attached character.
instance HasAbilities SelfPreservation where
  getAbilities (SelfPreservation a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a) | AttachedTo _ <- [a.placement]]

instance RunMessage SelfPreservation where
  runMessage msg s@(SelfPreservation a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      case a.placement of
        AttachedTo host -> push $ HealDamage (toSource a) host 2
        _ -> pure ()
      pure s
    _ -> SelfPreservation <$> runMessage msg a
