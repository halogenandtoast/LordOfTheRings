module LordOfTheRings.Attachment.Cards.MithrilShirtTreasure (mithrilShirtTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MithrilShirtTreasure = MithrilShirtTreasure AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure Mithril Shirt of The Lonely Mountain. Attach to Bilbo Baggins.
mithrilShirtTreasure :: AttachmentCard MithrilShirtTreasure
mithrilShirtTreasure =
  attachment MithrilShirtTreasure Cards.mithrilShirtTreasure & attachTo (AttachToHero $ HeroWithTitle "Bilbo Baggins")

-- | Attached hero gets +1 [defense] and +1 hit point.
instance HasModifiersFor MithrilShirtTreasure where
  getModifiersFor (MithrilShirtTreasure a) = withHost a \host -> modified a host [DefenseModifier 1, HitPointModifier 1]
