module LordOfTheRings.Event.Cards.OpenTheArmory (openTheArmory) where

import LordOfTheRings.Card (Card (PlayerCard), toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Trait (Trait (Armor, Weapon))

newtype OpenTheArmory = OpenTheArmory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

openTheArmory :: EventCard OpenTheArmory
openTheArmory = event OpenTheArmory Cards.openTheArmory

gear :: CardMatcher
gear = CardWithType AttachmentType <> CardMatchAny [CardWithTrait Weapon, CardWithTrait Armor]

-- | Action: Search the top 10 cards of your deck for a Weapon or Armor attachment and add
-- it to your hand. Shuffle your deck.
-- Valour Action: Search the top 5 cards of your deck for a Weapon or Armor attachment and
-- put it into play. Shuffle your deck.
instance RunMessage OpenTheArmory where
  runMessage msg e@(OpenTheArmory attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      chooseOrRunOneM pid do
        labeled "openTheArmory.toHand" $ searchDeck pid attrs 10 gear False
        valourLabeled pid do
          top <- take 5 . (.deck) <$> getPlayer pid
          attachable <- forMaybeM [pc | PlayerCard pc <- top, cardMatches gear pc] \pc -> do
            unique <- passesUnique (PlayerCard pc)
            hosts <- cardHosts (toCardCode pc)
            pure $ guard (unique && notNull hosts) $> (pc, hosts)
          chooseOneM pid do
            questionLabeled "openTheArmory.intoPlay"
            causedBy attrs
            showing top
            for_ attachable \(pc, hosts) -> targeting pc do
              removeFromDeck pid pc.id
              chooseOrRunOneM pid do
                questionLabeled "openTheArmory.host"
                causedBy pc
                targets hosts (attachOutOfPlay pid pc)
            labeled "openTheArmory.none" (pure ())
          shufflePlayerDeck pid
      pure e
    _ -> OpenTheArmory <$> runMessage msg attrs
