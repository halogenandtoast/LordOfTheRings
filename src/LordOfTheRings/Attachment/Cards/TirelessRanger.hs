module LordOfTheRings.Attachment.Cards.TirelessRanger (tirelessRanger) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype TirelessRanger = TirelessRanger AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Black Riders. Permanent, on its card def. Setup: Attach to a hero: as the
-- Campaign Pool records it on its hero.
tirelessRanger :: AttachmentCard TirelessRanger
tirelessRanger = attachment TirelessRanger Cards.tirelessRanger & attachTo (AttachToHero AnyHero)

-- | Attached hero gets +1 [defense] and gains the Ranger trait.
instance HasModifiersFor TirelessRanger where
  getModifiersFor (TirelessRanger a) = withHost a \host -> modified a host [DefenseModifier 1, GainTrait Ranger]
