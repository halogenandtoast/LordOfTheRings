module LordOfTheRings.Attachment.Cards.ChieftainsBrooch (chieftainsBrooch) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardDef)

newtype ChieftainsBrooch = ChieftainsBrooch AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign. Attach to a hero. Restricted.
chieftainsBrooch :: AttachmentCard ChieftainsBrooch
chieftainsBrooch = attachment ChieftainsBrooch Cards.chieftainsBrooch

-- | Each ally that shares a Trait with attached hero gets +1 willpower: the hero's traits
-- as they are, those it gained among them.
instance HasModifiersFor ChieftainsBrooch where
  getModifiersFor (ChieftainsBrooch a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      traits <- toList <$> (getTraits host . toCardDef =<< getHero hid)
      allies <- ordNub . concat <$> traverse (select . AllyWithTrait) traits
      for_ allies \ally -> modified a ally [WillpowerModifier 1]
    _ -> pure ()
