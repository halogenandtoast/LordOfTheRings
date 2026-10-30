module LordOfTheRings.Quest.Cards.ThaurdirsPursuit (thaurdirsPursuit) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (cluesOnCharacters, undeadEnemy)

newtype ThaurdirsPursuit = ThaurdirsPursuit QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

thaurdirsPursuit :: QuestCard ThaurdirsPursuit
thaurdirsPursuit = quest ThaurdirsPursuit Cards.thaurdirsPursuit

-- | Thaurdir's Pursuit has -5 quest points for each Clue objective attached to a character.
instance HasModifiersFor ThaurdirsPursuit where
  getModifiersFor (ThaurdirsPursuit a) = do
    clues <- cluesOnCharacters
    when (clues > 0) $ modified a a [QuestPointModifier (-5 * clues)]

thaurdir :: EnemyMatcher
thaurdir = EnemyWithCardCode (toCardCode Enemies.thaurdir)

-- | Thaurdir has no hit points remaining: its damage has reached them, as it is
-- Indestructible, or it is gone.
thaurdirSpent :: HasGame m => m Bool
thaurdirSpent =
  selectOne thaurdir >>= \case
    Nothing -> pure True
    Just eid -> (<=) <$> getEnemyHitPoints eid <*> getDamage (toTarget eid)

-- | When Revealed: Add Thaurdir to the staging area. Each player searches the encounter
-- deck and discard pile for an Undead enemy and adds it to the staging area. Shuffle the
-- encounter deck.
-- This stage cannot be defeated while Thaurdir has any hit points remaining. When this
-- stage is defeated, the players win the game.
instance RunMessage ThaurdirsPursuit where
  runMessage msg q@(ThaurdirsPursuit attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      ThaurdirsPursuit <$> runConditionalStage thaurdirSpent msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.thaurdir)
      eachPlayer \p -> searchEncounterDeckAndDiscard p attrs undeadEnemy
      shuffleEncounterDeck
      pure q
    -- damage dealt to Thaurdir may spend his last hit point
    CheckDefeated _ (EnemyTarget eid) -> do
      isThaurdir <- selectAny (EnemyWithId eid <> thaurdir)
      when isThaurdir $ checkStage attrs.id
      ThaurdirsPursuit <$> runConditionalStage thaurdirSpent msg attrs
    _ -> ThaurdirsPursuit <$> runConditionalStage thaurdirSpent msg attrs
