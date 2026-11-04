module LordOfTheRings.Attachment.Cards.SongOfHope (songOfHope) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype SongOfHope = SongOfHope AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def.
songOfHope :: AttachmentCard SongOfHope
songOfHope = attachment SongOfHope Cards.songOfHope

-- | Action: Spend 1 resource from attached hero's pool to give attached hero +1 willpower
-- until the end of the phase. (Limit 3 times per phase.)
instance HasAbilities SongOfHope where
  getAbilities (SongOfHope a) =
    [limited (GroupLimit PerPhase 3) $ mkAbility a 1 $ ActionAbility (ResourceCost host 1) | AttachedTo host <- [a.placement]]

instance RunMessage SongOfHope where
  runMessage msg s@(SongOfHope a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> phaseModifier a host (WillpowerModifier 1)
      pure s
    _ -> SongOfHope <$> runMessage msg a
