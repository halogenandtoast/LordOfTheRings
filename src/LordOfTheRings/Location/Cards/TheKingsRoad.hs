module LordOfTheRings.Location.Cards.TheKingsRoad (theKingsRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheKingsRoad = TheKingsRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theKingsRoad :: LocationCard TheKingsRoad
theKingsRoad = location TheKingsRoad Cards.theKingsRoad

-- | While any player has 3 or more cards in hand, The King's Road gets +3 quest points.
-- While any player has 5 or more cards in hand, The King's Road gains: "When faced with
-- the option to travel, the players must travel to The King's Road if able."
instance HasModifiersFor TheKingsRoad where
  getModifiersFor (TheKingsRoad a) = do
    whenM (selectAny $ PlayerWithCardsInHand 3) $ modified a a [QuestPointModifier 3]
    whenM (selectAny $ PlayerWithCardsInHand 5) $ modified a a [MustBeTraveledTo]
