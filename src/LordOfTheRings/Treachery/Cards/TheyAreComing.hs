module LordOfTheRings.Treachery.Cards.TheyAreComing (theyAreComing) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheyAreComing = TheyAreComing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theyAreComing :: TreacheryCard TheyAreComing
theyAreComing = treachery TheyAreComing Cards.theyAreComing

{- | Doomed 2. When Revealed: Shuffle the encounter discard pile into the encounter deck and
discard cards from the top of the encounter deck until X enemies are discarded. X is the
number of players in the game. Add each enemy discarded this way to the staging area. The
deck is read for the discards once it is shuffled, a step later.
-}
instance RunMessage TheyAreComing where
  runMessage msg t@(TheyAreComing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players enemyCard
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches enemyCard) cards) addToStagingFromDiscard
      pure t
    _ -> TheyAreComing <$> runMessage msg attrs
   where
    enemyCard = CardWithType EnemyType
