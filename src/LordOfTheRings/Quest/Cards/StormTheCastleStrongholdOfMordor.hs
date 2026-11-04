module LordOfTheRings.Quest.Cards.StormTheCastleStrongholdOfMordor (stormTheCastleStrongholdOfMordor) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (runStormTheCastle)

newtype StormTheCastleStrongholdOfMordor = StormTheCastleStrongholdOfMordor QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest: Storm the Castle, Stronghold of Mordor on its other side.
stormTheCastleStrongholdOfMordor :: QuestCard StormTheCastleStrongholdOfMordor
stormTheCastleStrongholdOfMordor = quest StormTheCastleStrongholdOfMordor Cards.stormTheCastleStrongholdOfMordor

-- | When Revealed: Discard cards from the top of the encounter deck until X locations are
-- discarded, where X is the number of players. Add each location discarded this way to the
-- staging area.
instance RunMessage StormTheCastleStrongholdOfMordor where
  runMessage msg q@(StormTheCastleStrongholdOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType LocationType) cards
      pure q
    _ -> StormTheCastleStrongholdOfMordor <$> runStormTheCastle msg attrs
