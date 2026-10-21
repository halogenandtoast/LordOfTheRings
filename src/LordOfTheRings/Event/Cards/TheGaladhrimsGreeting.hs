module LordOfTheRings.Event.Cards.TheGaladhrimsGreeting (theGaladhrimsGreeting) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheGaladhrimsGreeting = TheGaladhrimsGreeting EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Reduce one player's threat by 6, or reduce each player's threat by 2.
theGaladhrimsGreeting :: EventCard TheGaladhrimsGreeting
theGaladhrimsGreeting = event TheGaladhrimsGreeting Cards.theGaladhrimsGreeting

instance RunMessage TheGaladhrimsGreeting where
  runMessage msg e@(TheGaladhrimsGreeting attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      chooseOneM pid do
        questionLabeled "theGaladhrimsGreeting.title"
        causedBy attrs
        labeled "theGaladhrimsGreeting.one" $ choosePlayerM pid attrs \p -> lowerThreat p 6
        labeled "theGaladhrimsGreeting.each" $ eachPlayer \p -> lowerThreat p 2
      pure e
    _ -> TheGaladhrimsGreeting <$> runMessage msg attrs
