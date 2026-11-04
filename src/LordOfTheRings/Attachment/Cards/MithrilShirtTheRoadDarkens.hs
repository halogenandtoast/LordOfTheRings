module LordOfTheRings.Attachment.Cards.MithrilShirtTheRoadDarkens (mithrilShirtTheRoadDarkens) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (RingBearer))

newtype MithrilShirtTheRoadDarkens = MithrilShirtTheRoadDarkens AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Road Darkens. Attach to the Ring-bearer.
mithrilShirtTheRoadDarkens :: AttachmentCard MithrilShirtTheRoadDarkens
mithrilShirtTheRoadDarkens = attachment MithrilShirtTheRoadDarkens Cards.mithrilShirtTheRoadDarkens & attachTo (AttachToHero $ HeroWithTrait RingBearer)

-- | Attached hero gets +1 [defense] and +1 hit point.
instance HasModifiersFor MithrilShirtTheRoadDarkens where
  getModifiersFor (MithrilShirtTheRoadDarkens a) = withHost a \host -> modified a host [DefenseModifier 1, HitPointModifier 1]
