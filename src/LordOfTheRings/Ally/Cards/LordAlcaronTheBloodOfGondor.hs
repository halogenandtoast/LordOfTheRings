module LordOfTheRings.Ally.Cards.LordAlcaronTheBloodOfGondor (lordAlcaronTheBloodOfGondor) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype LordAlcaronTheBloodOfGondor = LordAlcaronTheBloodOfGondor AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
lordAlcaronTheBloodOfGondor :: AllyCard LordAlcaronTheBloodOfGondor
lordAlcaronTheBloodOfGondor = ally LordAlcaronTheBloodOfGondor Cards.lordAlcaronTheBloodOfGondor

-- | Action: Exhaust Lord Alcaron to return an enemy engaged with you to the staging area.
instance HasAbilities LordAlcaronTheBloodOfGondor where
  getAbilities (LordAlcaronTheBloodOfGondor a) =
    [restricted a 1 (exists $ EnemyEngagedWith ActivePlayer) $ ActionAbility (ExhaustCost $ toTarget a)]

-- | If Lord Alcaron leaves play, the players have lost the game.
instance RunMessage LordAlcaronTheBloodOfGondor where
  runMessage msg s@(LordAlcaronTheBloodOfGondor a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "lordAlcaronTheBloodOfGondor.return"
        causedBy a
        targets enemies \e -> placeEnemy e StagingArea
      pure s
    Discard _ (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> LordAlcaronTheBloodOfGondor <$> runMessage msg a
