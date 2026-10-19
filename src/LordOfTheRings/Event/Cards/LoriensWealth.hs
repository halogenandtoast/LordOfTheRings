module LordOfTheRings.Event.Cards.LoriensWealth (loriensWealth) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype LoriensWealth = LoriensWealth EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a player. That player draws 3 cards.
loriensWealth :: EventCard LoriensWealth
loriensWealth = event LoriensWealth Cards.loriensWealth

instance RunMessage LoriensWealth where
  runMessage msg e@(LoriensWealth attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> drawCards p 3
      pure e
    _ -> LoriensWealth <$> runMessage msg attrs
