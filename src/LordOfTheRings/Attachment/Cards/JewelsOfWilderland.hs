module LordOfTheRings.Attachment.Cards.JewelsOfWilderland (jewelsOfWilderland) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype JewelsOfWilderland = JewelsOfWilderland AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character.
jewelsOfWilderland :: AttachmentCard JewelsOfWilderland
jewelsOfWilderland = attachment JewelsOfWilderland Cards.jewelsOfWilderland & attachTo AttachToCharacter

-- | Attached character gets +1 [willpower].
instance HasModifiersFor JewelsOfWilderland where
  getModifiersFor (JewelsOfWilderland a) = withHost a \host -> modified a host [WillpowerModifier 1]

-- | Action: Exhaust Jewels of Wilderland and discard a card from your hand to draw a card.
instance HasAbilities JewelsOfWilderland where
  getAbilities (JewelsOfWilderland a) = [mkAbility a 1 $ ActionAbility (Costs [exhaustCost a, DiscardFromHandCost a.controller 1])]

instance RunMessage JewelsOfWilderland where
  runMessage msg s@(JewelsOfWilderland a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> JewelsOfWilderland <$> runMessage msg a
