{- | Thrór's Golden Cup, an Erebor treasure The Lonely Mountain's burgle attempts take: its card and
where it attaches. Its game text is the Hobbit player cards' to write.
-}
module LordOfTheRings.Attachment.Cards.ThrorsGoldenCup (throrsGoldenCup) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThrorsGoldenCup = ThrorsGoldenCup AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
throrsGoldenCup :: AttachmentCard ThrorsGoldenCup
throrsGoldenCup = attachment ThrorsGoldenCup Cards.throrsGoldenCup & attachTo (AttachToHero AnyHero)
