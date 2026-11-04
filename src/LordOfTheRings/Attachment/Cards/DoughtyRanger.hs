module LordOfTheRings.Attachment.Cards.DoughtyRanger (doughtyRanger) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype DoughtyRanger = DoughtyRanger AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def.
doughtyRanger :: AttachmentCard DoughtyRanger
doughtyRanger = attachment DoughtyRanger Cards.doughtyRanger

-- | Attached hero gains the Ranger trait.
instance HasModifiersFor DoughtyRanger where
  getModifiersFor (DoughtyRanger a) = withHost a \host -> modified a host [GainTrait Ranger]

-- | Response: After you play Doughty Ranger from your hand, draw a card.
instance HasAbilities DoughtyRanger where
  getAbilities (DoughtyRanger a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage DoughtyRanger where
  runMessage msg s@(DoughtyRanger a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> DoughtyRanger <$> runMessage msg a
