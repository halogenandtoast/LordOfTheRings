{- | Thrór's Hunting Bow, an Erebor treasure The Lonely Mountain's burgle attempts take: its card and
where it attaches. Its game text is the Hobbit player cards' to write.
-}
module LordOfTheRings.Attachment.Cards.ThrorsHuntingBow (throrsHuntingBow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThrorsHuntingBow = ThrorsHuntingBow AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
throrsHuntingBow :: AttachmentCard ThrorsHuntingBow
throrsHuntingBow = attachment ThrorsHuntingBow Cards.throrsHuntingBow & attachTo (AttachToHero AnyHero)
