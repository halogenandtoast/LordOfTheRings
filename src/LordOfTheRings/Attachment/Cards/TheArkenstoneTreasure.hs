{- | The Arkenstone, an Erebor treasure The Lonely Mountain's burgle attempts take: its card and
where it attaches. Its game text is the Hobbit player cards' to write.
-}
module LordOfTheRings.Attachment.Cards.TheArkenstoneTreasure (theArkenstoneTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype TheArkenstoneTreasure = TheArkenstoneTreasure AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
theArkenstoneTreasure :: AttachmentCard TheArkenstoneTreasure
theArkenstoneTreasure = attachment TheArkenstoneTreasure Cards.theArkenstoneTreasure & attachTo (AttachToHero AnyHero)
