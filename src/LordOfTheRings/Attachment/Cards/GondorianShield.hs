module LordOfTheRings.Attachment.Cards.GondorianShield (gondorianShield) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Trait (Trait (Gondor))

newtype GondorianShield = GondorianShield AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Restricted. Limit 1 per hero.
gondorianShield :: AttachmentCard GondorianShield
gondorianShield = attachment GondorianShield Cards.gondorianShield

-- | Attached hero gains +1 defense. (+2 defense instead if attached hero has the Gondor
-- trait.)
instance HasModifiersFor GondorianShield where
  getModifiersFor (GondorianShield a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      gondor <- hasTrait Gondor host . toCardDef =<< getHero hid
      modified a host [DefenseModifier $ if gondor then 2 else 1]
    _ -> pure ()
