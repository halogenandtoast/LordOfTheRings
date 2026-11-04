{- | Thrór's Battle Axe, an Erebor treasure The Lonely Mountain's burgle attempts take: its card and
where it attaches. Its game text is the Hobbit player cards' to write.
-}
module LordOfTheRings.Attachment.Cards.ThrorsBattleAxe (throrsBattleAxe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThrorsBattleAxe = ThrorsBattleAxe AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
throrsBattleAxe :: AttachmentCard ThrorsBattleAxe
throrsBattleAxe = attachment ThrorsBattleAxe Cards.throrsBattleAxe & attachTo (AttachToHero AnyHero)
