module LordOfTheRings.Attachment.Cards.DwarfPipe (dwarfPipe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Trait (Trait (Dwarf, Pipe))

newtype DwarfPipe = DwarfPipe AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf character. Limit 1 per character.
--
-- ponytail: the limit is read as no Pipe already attached; Dwarf Pipe is the only Pipe a
-- Dwarf can take so far
dwarfPipe :: AttachmentCard DwarfPipe
dwarfPipe =
  attachment DwarfPipe Cards.dwarfPipe
    & attachTo
      ( AttachToCharacterMatching
          (HeroWithTrait Dwarf <> not_ (HeroWithAttached Pipe))
          (AllyWithTrait Dwarf <> not_ (AllyWithAttached Pipe))
      )

-- | Response: After a card is discarded from your deck, exhaust Dwarf Pipe to place that
-- card on the bottom of your deck.
instance HasAbilities DwarfPipe where
  getAbilities (DwarfPipe a) =
    [mkAbility a 1 $ ResponseAbility (DiscardedFromDeckWindow After a.owner Nothing) (ExhaustCost $ toTarget a)]

instance RunMessage DwarfPipe where
  runMessage msg s@(DwarfPipe a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      discarded <- (.discard) <$> getPlayer pid
      let ids = [cid | Window _ (DiscardedFromDeck _ cid) <- windows]
          cards = [pc | PlayerCard pc <- discarded, pc.id `elem` ids]
      chooseOrRunOneM pid do
        questionLabeled "dwarfPipe.chooseCard"
        causedBy a
        showing (map PlayerCard cards)
        targets cards \pc -> putDiscardOnBottomOfDeck pid pc.id
      pure s
    _ -> DwarfPipe <$> runMessage msg a
