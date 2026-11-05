module LordOfTheRings.Attachment.Cards.GlitteringLute (glitteringLute) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype GlitteringLute = GlitteringLute AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character. Restricted, on its card def.
glitteringLute :: AttachmentCard GlitteringLute
glitteringLute = attachment GlitteringLute Cards.glitteringLute & attachTo AttachToCharacter

-- | Attached character gets +2 [willpower].
instance HasModifiersFor GlitteringLute where
  getModifiersFor (GlitteringLute a) = withHost a \host -> modified a host [WillpowerModifier 2]

-- | Response: After you play Glittering Lute, each player may discard a card from their
-- hand. Each player who does so reduces their threat by 2.
instance HasAbilities GlitteringLute where
  getAbilities (GlitteringLute a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage GlitteringLute where
  runMessage msg s@(GlitteringLute a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        hand <- (.hand) <$> getPlayer p
        chooseOneM p do
          questionLabeled "glitteringLute.discard"
          causedBy a
          targets hand \c -> do
            discardFromHand p c
            lowerThreat p 2
          labeled "glitteringLute.decline" (pure ())
      pure s
    _ -> GlitteringLute <$> runMessage msg a
