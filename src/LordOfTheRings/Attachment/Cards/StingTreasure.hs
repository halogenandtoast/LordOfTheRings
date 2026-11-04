module LordOfTheRings.Attachment.Cards.StingTreasure (stingTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype StingTreasure = StingTreasure AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A treasure of The Hobbit, discovered in We Must Away, Ere Break of Day. Attach to
-- Bilbo Baggins.
stingTreasure :: AttachmentCard StingTreasure
stingTreasure =
  attachment StingTreasure Cards.stingTreasure & attachTo (AttachToHero $ HeroWithTitle "Bilbo Baggins")

instance RunMessage StingTreasure where
  runMessage msg (StingTreasure a) = StingTreasure <$> runMessage msg a
