module LordOfTheRings.Ally.Cards.FaramirObjective (faramirObjective) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, turnFaceup)

newtype FaramirObjective = FaramirObjective AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
faramirObjective :: AllyCard FaramirObjective
faramirObjective = ally FaramirObjective Cards.faramirObjective

-- | Combat Action: Exhaust Faramir to turn 1 hidden card faceup. If that card is an enemy,
-- deal 3 damage to it.
-- ponytail: offered with no hidden card in the game too, when it only exhausts him
instance HasAbilities FaramirObjective where
  getAbilities (FaramirObjective a) =
    [restricted a 1 (DuringPhase CombatPhase) $ ActionAbility (ExhaustCost $ toTarget a)]

-- | The player chooses whose hidden card turns faceup, which engages that player if it is
-- an enemy.
-- If Faramir leaves play, the players have lost the game.
instance RunMessage FaramirObjective where
  runMessage msg s@(FaramirObjective a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      players <- filterM (fmap notNull . getHiddenCards) =<< getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "faramirObjective.turnFaceup"
        causedBy a
        targets players \p -> do
          -- ponytail: the first taken; they are facedown, so there is nothing to choose between
          hidden <- take 1 <$> getHiddenCards p
          for_ hidden \card -> turnFaceup p card >>= traverse_ \e -> dealDamage a e 3
      pure s
    Discard (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> FaramirObjective <$> runMessage msg a
