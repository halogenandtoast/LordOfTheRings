module LordOfTheRings.Attachment.Cards.VisionaryLeadership (visionaryLeadership) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype VisionaryLeadership = VisionaryLeadership AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

visionaryLeadership :: AttachmentCard VisionaryLeadership
visionaryLeadership = attachment VisionaryLeadership Cards.visionaryLeadership & attachTo (AttachToHero $ HeroWithTrait Gondor)

-- | While attached hero has at least 1 resource in its resource pool, Gondor characters
-- get +1 willpower.
instance HasModifiersFor VisionaryLeadership where
  getModifiersFor (VisionaryLeadership a) = case a.placement of
    AttachedTo (HeroTarget hid) -> do
      rich <- (> 0) . (.resources) <$> getHero hid
      when rich do
        gondor <- selectCharacters (HeroWithTrait Gondor) (AllyWithTrait Gondor)
        for_ gondor \c -> modified a c [WillpowerModifier 1]
    _ -> pure ()
