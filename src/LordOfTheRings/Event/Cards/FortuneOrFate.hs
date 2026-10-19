module LordOfTheRings.Event.Cards.FortuneOrFate (fortuneOrFate) where

import LordOfTheRings.Card (Card (..), CardDef (..), toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype FortuneOrFate = FortuneOrFate EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a hero in any player's discard pile. Put that card into play, under its
-- owner's control.
fortuneOrFate :: EventCard FortuneOrFate
fortuneOrFate = event FortuneOrFate Cards.fortuneOrFate

instance RunMessage FortuneOrFate where
  runMessage msg e@(FortuneOrFate attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      order <- getPlayerOrder
      heroes <- fmap concat $ for order \owner -> do
        discarded <- (.discard) <$> getPlayer owner
        pure [(owner, pc) | PlayerCard pc <- discarded, cdCardType (toCardDef pc) == HeroType]
      chooseOneM pid do
        questionLabeled "fortuneOrFate.choose"
        causedBy attrs
        for_ heroes \(owner, pc) -> targeting pc do
          removeFromDiscard owner pc.id
          push $ PutHeroIntoPlay owner pc
      pure e
    _ -> FortuneOrFate <$> runMessage msg attrs
