module LordOfTheRings.Objective.Cards.MinasTirith (minasTirith) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (minasTirithHitPoints)

newtype MinasTirith = MinasTirith ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Minas Tirith has 30 hit points plus 10 for each player (see 'minasTirithHitPoints').
minasTirith :: ObjectiveCard MinasTirith
minasTirith = objective MinasTirith Cards.minasTirith

-- | Forced: At the end of the round, deal 1 damage to Minas Tirith for each enemy in the
-- staging area. Then, if Minas Tirith has no hit points remaining, the players lose the
-- game.
instance HasAbilities MinasTirith where
  getAbilities (MinasTirith a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage MinasTirith where
  runMessage msg o@(MinasTirith attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      enemies <- length <$> select EnemyInStagingArea
      dealDamage attrs attrs enemies
      doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      hp <- minasTirithHitPoints
      when (countTokens Damage attrs.objectiveTokens >= hp) $ gameOver Lost
      pure o
    _ -> MinasTirith <$> runMessage msg attrs
