module LordOfTheRings.Quest.Cards.StormTheCastleCastleGarrison (stormTheCastleCastleGarrison) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (runStormTheCastle)

newtype StormTheCastleCastleGarrison = StormTheCastleCastleGarrison QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest: Storm the Castle, Castle Garrison on its other side.
stormTheCastleCastleGarrison :: QuestCard StormTheCastleCastleGarrison
stormTheCastleCastleGarrison = quest StormTheCastleCastleGarrison Cards.stormTheCastleCastleGarrison

-- | When Revealed: Discard cards from the top of the encounter deck until X enemies are
-- discarded, where X is the number of players. Add each enemy discarded this way to the
-- staging area.
instance RunMessage StormTheCastleCastleGarrison where
  runMessage msg q@(StormTheCastleCastleGarrison attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType EnemyType) cards
      pure q
    _ -> StormTheCastleCastleGarrison <$> runStormTheCastle msg attrs
