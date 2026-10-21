module LordOfTheRings.Attachment.Cards.Lembas (lembas) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Lembas = Lembas AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play only if you control a Noldor or Silvan hero. Attach to a hero.
--
-- ponytail: playable without a Noldor or Silvan hero; attachments have no play
-- criteria of their own yet.
lembas :: AttachmentCard Lembas
lembas = attachment Lembas Cards.lembas

-- | Action: Discard Lembas to ready attached hero and heal 3 damage from it.
instance HasAbilities Lembas where
  getAbilities (Lembas a) = [mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

instance RunMessage Lembas where
  runMessage msg s@(Lembas a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      withHost a \host -> do
        ready host
        healDamage a host 3
      pure s
    _ -> Lembas <$> runMessage msg a
