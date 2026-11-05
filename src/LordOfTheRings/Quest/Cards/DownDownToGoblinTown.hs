module LordOfTheRings.Quest.Cards.DownDownToGoblinTown (downDownToGoblinTown) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim.Helpers (greatGoblinDefeated, switchEncounterDeck)

newtype DownDownToGoblinTown = DownDownToGoblinTown QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

downDownToGoblinTown :: QuestCard DownDownToGoblinTown
downDownToGoblinTown = quest DownDownToGoblinTown Cards.downDownToGoblinTown

-- | Players cannot defeat this stage unless The Great Goblin is in the victory display.
-- If the players defeat this stage, they have won the game: it is the last stage.
instance HasModifiersFor DownDownToGoblinTown where
  getModifiersFor (DownDownToGoblinTown a) = when a.flipped do
    unlessM greatGoblinDefeated $ modifySelf a [CannotBeDefeated]

{- | When Revealed (side A): Shuffle all encounter cards back into the encounter deck and
set it aside, inactive. The second encounter deck becomes the active encounter deck.
Search the encounter deck for The Great Goblin and add it to the staging area. Then,
shuffle the encounter deck.

The encounter cards in play are shuffled in, and the deck and its discard pile set aside
with them (rules insert, "Active and Inactive Encounter Decks"); the victory display
stays.

When Revealed (side B): Reveal 3 encounter cards per player. Bilbo Baggins may spend X
resources to reduce the total number of encounter cards revealed by X. (To a minimum of
1.) His controller decides how many to spend.
-}
instance RunMessage DownDownToGoblinTown where
  runMessage msg q@(DownDownToGoblinTown attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DownDownToGoblinTown <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- select AnyEnemy
      locations <- select AnyLocation
      treacheries <- getsGame (map toTarget . toList . (.entities.treacheries))
      objectives <- getsGame (map toTarget . toList . (.entities.objectives))
      for_ (map toTarget enemies <> map toTarget locations <> treacheries <> objectives) shuffleIntoEncounterDeck
      switchEncounterDeck
      lead <- getLead
      searchEncounterDeckToStaging lead (CardWithTitle "The Great Goblin")
      players <- length <$> getRemainingPlayers
      let total = 3 * players
      resources <- getBagginsResources
      scenario <- getsGame (toAttrs . (.scenario))
      -- Nightmare setup caps the resources spent on this revelation at the player count.
      let available = if scenario.nightmare then min players resources else resources
      getBilbo >>= \case
        Just bilbo | resources > 0 && total > 1 ->
          chooseAmountM lead "downDownToGoblinTown.spend" attrs 0 (min available (total - 1)) \x -> do
            when (x > 0) $ spendResourcesFrom lead bilbo x
            replicateM_ (total - x) revealEncounterCard
        _ -> replicateM_ total revealEncounterCard
      pure q
    _ -> DownDownToGoblinTown <$> runMessage msg attrs
