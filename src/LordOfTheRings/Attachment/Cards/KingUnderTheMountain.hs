module LordOfTheRings.Attachment.Cards.KingUnderTheMountain (kingUnderTheMountain) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype KingUnderTheMountain = KingUnderTheMountain AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf hero.
kingUnderTheMountain :: AttachmentCard KingUnderTheMountain
kingUnderTheMountain =
  attachment KingUnderTheMountain Cards.kingUnderTheMountain & attachTo (AttachToHero $ HeroWithTrait Dwarf)

-- | Action: Exhaust King Under the Mountain to look at the top 2 cards of your deck. Add
-- 1 to your hand and discard the other.
instance HasAbilities KingUnderTheMountain where
  getAbilities (KingUnderTheMountain a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage KingUnderTheMountain where
  runMessage msg s@(KingUnderTheMountain a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- take 2 . (.deck) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "kingUnderTheMountain.take"
        causedBy a
        showing top
        targets [pc | PlayerCard pc <- top] \pc -> do
          push $ TakeFromDeck pid pc.id
          -- the other card is on top now
          discardTopOfDeck pid (length top - 1)
      pure s
    _ -> KingUnderTheMountain <$> runMessage msg a
