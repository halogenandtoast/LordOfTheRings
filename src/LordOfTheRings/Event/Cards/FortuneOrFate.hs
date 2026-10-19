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
      choices <- fmap concat $ for order \owner -> do
        discard <- (.discard) <$> getPlayer owner
        pure
          [ targetLabel (CardIdTarget pc.id) [RemoveFromDiscard owner pc.id, PutHeroIntoPlay owner pc]
          | PlayerCard pc <- discard
          , cdCardType (toCardDef pc) == HeroType
          ]
      unless (null choices) $ push $ questionLabelWith (ikey "label.fortuneOrFate.choose") (Just $ toTarget attrs) [] pid choices
      pure e
    _ -> FortuneOrFate <$> runMessage msg attrs
