{- | Mithril Shirt, an Erebor treasure The Lonely Mountain's burgle attempts take: its card and
where it attaches. Its game text is the Hobbit player cards' to write.
-}
module LordOfTheRings.Attachment.Cards.MithrilShirtTreasure (mithrilShirtTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MithrilShirtTreasure = MithrilShirtTreasure AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Bilbo Baggins.
mithrilShirtTreasure :: AttachmentCard MithrilShirtTreasure
mithrilShirtTreasure = attachment MithrilShirtTreasure Cards.mithrilShirtTreasure & attachTo (AttachToHero $ HeroWithTitle "Bilbo Baggins")
