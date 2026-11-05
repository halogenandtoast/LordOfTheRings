module LordOfTheRings.Attachment.Cards.JewelsOfWilderland (jewelsOfWilderland) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype JewelsOfWilderland = JewelsOfWilderland AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ered Mithrin's treasure. Attach to a character.
jewelsOfWilderland :: AttachmentCard JewelsOfWilderland
jewelsOfWilderland = attachment JewelsOfWilderland Cards.jewelsOfWilderland & attachTo AttachToCharacter

-- | Attached character gets +1 [willpower].
instance HasModifiersFor JewelsOfWilderland where
  getModifiersFor (JewelsOfWilderland a) = withHost a \host -> modified a host [WillpowerModifier 1]

-- | Action: Exhaust Jewels of Wilderland and discard a card from your hand to draw a card.
instance HasAbilities JewelsOfWilderland where
  getAbilities (JewelsOfWilderland a) = case a.placement of
    AttachedTo _ -> [mkAbility a 1 $ ActionAbility (Costs [ExhaustCost (toTarget a), DiscardFromHandCost a.controller 1])]
    _ -> []

instance RunMessage JewelsOfWilderland where
  runMessage msg j@(JewelsOfWilderland a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure j
    _ -> JewelsOfWilderland <$> runMessage msg a
