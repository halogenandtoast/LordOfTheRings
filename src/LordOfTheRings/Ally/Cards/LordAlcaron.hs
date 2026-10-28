module LordOfTheRings.Ally.Cards.LordAlcaron (lordAlcaron) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype LordAlcaron = LordAlcaron AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
lordAlcaron :: AllyCard LordAlcaron
lordAlcaron = ally LordAlcaron Cards.lordAlcaron

-- | Action: Exhaust Lord Alcaron to return an enemy engaged with you to the staging area.
instance HasAbilities LordAlcaron where
  getAbilities (LordAlcaron a) =
    [restricted a 1 (exists $ EnemyEngagedWith ActivePlayer) $ ActionAbility (ExhaustCost $ toTarget a)]

-- | If Lord Alcaron leaves play, the players have lost the game.
instance RunMessage LordAlcaron where
  runMessage msg s@(LordAlcaron a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "lordAlcaron.return"
        causedBy a
        targets enemies \e -> placeEnemy e StagingArea
      pure s
    Discard (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> LordAlcaron <$> runMessage msg a
