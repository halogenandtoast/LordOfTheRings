module LordOfTheRings.Quest.Cards.DefeatTheSerpent (defeatTheSerpent) where

import LordOfTheRings.Card (IsCard (toCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (haradStage, randomHaradObjective)

newtype DefeatTheSerpent = DefeatTheSerpent QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

defeatTheSerpent :: QuestCard DefeatTheSerpent
defeatTheSerpent = quest DefeatTheSerpent Cards.defeatTheSerpent

-- | If a Harad objective is not attached, the first player claims it and attaches it to a
-- hero he controls. This stage cannot be defeated unless each Harad objective in play is
-- attached to a hero. If the players defeat this stage, they win the game: it is the last.
-- While there are at least 6 progress tokens here, The Black Serpent loses its "Cannot
-- leave the staging area or take damage": its card reads this stage.
instance HasModifiersFor DefeatTheSerpent where
  getModifiersFor (DefeatTheSerpent a) = when a.flipped $ haradStage a

-- | When Revealed: Randomly select 1 set aside Harad objective and attach it to The Black
-- Serpent. Shuffle the encounter discard pile into the encounter deck and reveal 1
-- encounter card per player.
instance RunMessage DefeatTheSerpent where
  runMessage msg q@(DefeatTheSerpent attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DefeatTheSerpent <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      serpent <- selectOne (EnemyWithCardCode $ toCardCode Enemies.theBlackSerpent)
      for_ serpent \s -> do
        cid <- (.id) . toCard <$> getEnemy s
        traverse_ (`putObjectiveIntoPlayGuardedBy` cid) =<< randomHaradObjective
      shuffleEncounterDiscardIntoDeck AnyCard
      players <- length <$> getRemainingPlayers
      revealEncounterCards players
      pure q
    _ -> DefeatTheSerpent <$> runMessage msg attrs
