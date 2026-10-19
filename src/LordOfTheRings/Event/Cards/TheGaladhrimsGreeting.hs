module LordOfTheRings.Event.Cards.TheGaladhrimsGreeting (theGaladhrimsGreeting) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheGaladhrimsGreeting = TheGaladhrimsGreeting EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Reduce one player's threat by 6, or reduce each player's threat by 2.
theGaladhrimsGreeting :: EventCard TheGaladhrimsGreeting
theGaladhrimsGreeting = event TheGaladhrimsGreeting Cards.theGaladhrimsGreeting

instance RunMessage TheGaladhrimsGreeting where
  runMessage msg e@(TheGaladhrimsGreeting attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      order <- getPlayerOrder
      one <- choosePlayer pid (Just $ toTarget attrs) \p -> [LowerThreat p 6]
      push
        $ questionLabelWith (ikey "label.theGaladhrimsGreeting.title") (Just $ toTarget attrs) [] pid
          [ Label (ikey "label.theGaladhrimsGreeting.one") [one]
          , Label (ikey "label.theGaladhrimsGreeting.each") [LowerThreat p 2 | p <- order]
          ]
      pure e
    _ -> TheGaladhrimsGreeting <$> runMessage msg attrs
