module LordOfTheRings.Event.Cards.TimelyAid (timelyAid) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TimelyAid = TimelyAid EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 3, on its card def.
-- Action: Reveal the top 5 cards of your deck and put 1 revealed ally into play, if
-- able. Shuffle all other revealed cards back into your deck.
timelyAid :: EventCard TimelyAid
timelyAid = event TimelyAid Cards.timelyAid

instance RunMessage TimelyAid where
  runMessage msg e@(TimelyAid attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      searchDeckIntoPlay pid attrs 5 (CardWithType AllyType) 1 []
      pure e
    _ -> TimelyAid <$> runMessage msg attrs
