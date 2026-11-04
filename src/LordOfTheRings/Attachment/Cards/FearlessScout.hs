module LordOfTheRings.Attachment.Cards.FearlessScout (fearlessScout) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Scout))

newtype FearlessScout = FearlessScout AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def.
fearlessScout :: AttachmentCard FearlessScout
fearlessScout = attachment FearlessScout Cards.fearlessScout

-- | Attached hero gains the Scout trait.
instance HasModifiersFor FearlessScout where
  getModifiersFor (FearlessScout a) = withHost a \host -> modified a host [GainTrait Scout]

-- | Response: After you play Fearless Scout from your hand, draw a card.
instance HasAbilities FearlessScout where
  getAbilities (FearlessScout a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage FearlessScout where
  runMessage msg s@(FearlessScout a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> FearlessScout <$> runMessage msg a
