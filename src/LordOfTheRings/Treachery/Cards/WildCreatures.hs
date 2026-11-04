module LordOfTheRings.Treachery.Cards.WildCreatures (wildCreatures) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Creature))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

-- | Roam Across Rhovanion's treachery, not the Wild Creatures encounter set.
newtype WildCreatures = WildCreatures TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildCreatures :: TreacheryCard WildCreatures
wildCreatures = treachery WildCreatures Cards.wildCreatures

creatureEnemy :: CardMatcher
creatureEnemy = CardWithType EnemyType <> CardWithTrait Creature

{- | When Revealed: Starting with the first player, each player chooses an enemy in the
staging area and engages it. Then, if any player did not engage an enemy this way, discard
cards from the encounter deck until a Creature enemy is discarded and add it to the staging
area.

Each player in turn order is one step, the staging area read as his turn comes, after what
the one before engaged has resolved; step 100 and up remembers that a player engaged none.
-}
instance RunMessage WildCreatures where
  runMessage msg t@(WildCreatures attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 0 msg
      pure t
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      let (i, missed) = (n `mod` 100, n >= 100)
      case drop i players of
        p : _ -> do
          enemies <- select EnemyInStagingArea
          chooseOneM p do
            questionLabeled "wildCreatures.engage"
            causedBy attrs
            targets enemies \e -> do
              engageEnemy p e
              doStep (n + 1) inner
          when (null enemies) $ doStep (i + 101) inner
        [] -> when missed $ discardEncounterCardsUntil attrs creatureEnemy
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (find (cardMatches creatureEnemy) cards)
      pure t
    _ -> WildCreatures <$> runMessage msg attrs
