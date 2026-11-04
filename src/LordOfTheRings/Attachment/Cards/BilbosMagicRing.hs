{- | Bilbo's Magic Ring, the treasure Flies and Spiders lets each player shuffle into his
deck: its card and where it attaches, for that scenario. Its game text is the Hobbit
player cards' to write.
-}
module LordOfTheRings.Attachment.Cards.BilbosMagicRing (bilbosMagicRing) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype BilbosMagicRing = BilbosMagicRing AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Bilbo Baggins.
bilbosMagicRing :: AttachmentCard BilbosMagicRing
bilbosMagicRing = attachment BilbosMagicRing Cards.bilbosMagicRing & attachTo (AttachToHero $ HeroWithTitle "Bilbo Baggins")
