module LordOfTheRings.Quest.Cards.TheWallIsBreached (theWallIsBreached) where

import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (addDiscardedEnemiesToStaging, discardUntilEnemiesPerPlayer)

newtype TheWallIsBreached = TheWallIsBreached QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Defense, as its card def says.
theWallIsBreached :: QuestCard TheWallIsBreached
theWallIsBreached = quest TheWallIsBreached Cards.theWallIsBreached

-- | Forced: After a character is destroyed, place 1 progress on this stage (bypassing any
-- active location).
instance HasAbilities TheWallIsBreached where
  getAbilities (TheWallIsBreached a) = [mkAbility a 1 $ forced $ CharacterDestroyedWindow After | a.flipped]

{- | When Revealed: Shuffle the encounter discard pile into the encounter deck. Discard
cards from the encounter deck until X enemies are discarded. X is the number of players.
Add each enemy discarded this way to the staging area.

If this stage is defeated, the players lose the game: the encounter deck has taken Helm's
Deep, so the stage never advances (MEC45 rules, "Defense").
-}
instance RunMessage TheWallIsBreached where
  runMessage msg q@(TheWallIsBreached attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheWallIsBreached <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      discardUntilEnemiesPerPlayer attrs
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      addDiscardedEnemiesToStaging cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress attrs attrs 1
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      let attrs' = attrs & tokensL %~ addTokens Progress n
      lost <- hasEnoughProgress attrs'
      when lost $ gameOver Lost
      pure $ TheWallIsBreached attrs'
    CheckStage qid | qid == attrs.id -> do
      lost <- hasEnoughProgress attrs
      when lost $ gameOver Lost
      pure q
    _ -> TheWallIsBreached <$> runMessage msg attrs
