module LordOfTheRings.Attachment.Cards.DiligentNoble (diligentNoble) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noble))

newtype DiligentNoble = DiligentNoble AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def.
diligentNoble :: AttachmentCard DiligentNoble
diligentNoble = attachment DiligentNoble Cards.diligentNoble

-- | Attached hero gains the Noble trait.
instance HasModifiersFor DiligentNoble where
  getModifiersFor (DiligentNoble a) = withHost a \host -> modified a host [GainTrait Noble]

-- | Response: After you play Diligent Noble from your hand, draw a card.
instance HasAbilities DiligentNoble where
  getAbilities (DiligentNoble a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage DiligentNoble where
  runMessage msg s@(DiligentNoble a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> DiligentNoble <$> runMessage msg a
