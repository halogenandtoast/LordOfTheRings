module LordOfTheRings.Quest.Cards.OrdersFromOrthanc (ordersFromOrthanc) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (setUpOutOfPlayDecks)
import LordOfTheRings.Trait (Trait (Mountain))

newtype OrdersFromOrthanc = OrdersFromOrthanc QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ordersFromOrthanc :: QuestCard OrdersFromOrthanc
ordersFromOrthanc = quest OrdersFromOrthanc Cards.ordersFromOrthanc

-- | Setup: Each player removes the top 20 cards of his deck and places them aside, as an
-- out-of-play deck. The first player takes Mugash plus 1 Mugash's Guard for each other
-- player in the game and randomly shuffles one facedown into each player's out-of-play
-- deck. Place any remaining copies of Mugash's Guard aside, out of play. (The scenario
-- keeps the out-of-play decks.)
-- When Revealed: Each player searches the encounter deck for a Mountain location and adds
-- it to the staging area. Shuffle the encounter deck. Then, advance to stage 2.
instance RunMessage OrdersFromOrthanc where
  runMessage msg q@(OrdersFromOrthanc attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      setUpOutOfPlayDecks
      askToResolveWhenRevealed attrs
      pure $ OrdersFromOrthanc $ attrs {questFlipped = True}
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckToStaging p (CardWithType LocationType <> CardWithTrait Mountain)
      completeQuest attrs.id
      pure q
    _ -> OrdersFromOrthanc <$> runMessage msg attrs
