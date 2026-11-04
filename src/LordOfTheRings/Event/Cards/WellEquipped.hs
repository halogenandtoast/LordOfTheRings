module LordOfTheRings.Event.Cards.WellEquipped (wellEquipped) where

import LordOfTheRings.Card (Card (PlayerCard), toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Trait (Trait (Dwarf))

newtype WellEquipped = WellEquipped EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Discard the top 2 cards of your deck. You may attach 1 attachment card
-- discarded by this effect to an eligible Dwarf character in play.
wellEquipped :: EventCard WellEquipped
wellEquipped = event WellEquipped Cards.wellEquipped

instance RunMessage WellEquipped where
  runMessage msg e@(WellEquipped attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- take 2 . (.deck) <$> getPlayer pid
      dwarves <- selectCharacters (HeroWithTrait Dwarf) (AllyWithTrait Dwarf)
      -- the cards are known before they are discarded, so the choice is built now and
      -- asked once they are in the discard pile
      attachable <- forMaybeM [pc | PlayerCard pc <- top, cardMatches (CardWithType AttachmentType) pc] \pc -> do
        unique <- passesUnique (PlayerCard pc)
        hosts <- filter (`elem` dwarves) <$> cardHosts pid (toCardCode pc)
        pure $ guard (unique && notNull hosts) $> (pc, hosts)
      discardTopOfDeck pid 2
      unless (null attachable) $ chooseOneM pid do
        questionLabeled "wellEquipped.chooseAttachment"
        causedBy attrs
        for_ attachable \(pc, hosts) -> targeting pc do
          chooseOrRunOneM pid do
            questionLabeled "wellEquipped.chooseDwarf"
            causedBy pc
            targets hosts (attachFromDiscard pid pc)
        labeled "wellEquipped.none" (pure ())
      pure e
    _ -> WellEquipped <$> runMessage msg attrs
