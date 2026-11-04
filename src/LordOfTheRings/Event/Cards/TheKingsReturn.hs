module LordOfTheRings.Event.Cards.TheKingsReturn (theKingsReturn) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheKingsReturn = TheKingsReturn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

theKingsReturn :: EventCard TheKingsReturn
theKingsReturn = event TheKingsReturn Cards.theKingsReturn

-- | Action: Search your deck for a Guarded attachment and put it into play. Shuffle your
-- deck.
instance RunMessage TheKingsReturn where
  runMessage msg e@(TheKingsReturn attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      deck <- (.deck) <$> getPlayer pid
      guarded <- filterM (passesUnique . PlayerCard) [pc | PlayerCard pc <- deck, cardMatches (CardWithType AttachmentType <> CardWithGuarded) pc]
      chooseOneM pid do
        questionLabeled "theKingsReturn.choose"
        causedBy attrs
        showing deck
        for_ guarded \pc -> targeting pc do
          removeFromDeck pid pc.id
          putAttachmentIntoPlay pid pc
        labeled "theKingsReturn.none" (pure ())
      shufflePlayerDeck pid
      pure e
    _ -> TheKingsReturn <$> runMessage msg attrs
