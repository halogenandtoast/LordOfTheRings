module LordOfTheRings.Objective.Cards.ArnorRavaged (arnorRavaged) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype ArnorRavaged = ArnorRavaged ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Angmar Awakened campaign, Protect the Innocent's other side. Setup: Add
-- Arnor Ravaged to the staging area with damage on it equal to the amount of damage marked
-- for it in the Campaign Log, which the scenario does.
arnorRavaged :: ObjectiveCard ArnorRavaged
arnorRavaged = objective ArnorRavaged Cards.arnorRavaged

-- | Forced: After an attack damages a character but does not destroy it, move 1 damage
-- from Arnor Ravaged to that character: once the damage is placed and the character still
-- stands.
instance RunMessage ArnorRavaged where
  runMessage msg s@(ArnorRavaged a) = case msg of
    DoStep 1 (DealDamage (EnemyAttackSource _) target _) | isCharacter target, countTokens Damage a.objectiveTokens > 0 -> do
      whileInPlay target do
        removeTokens a a Damage 1
        placeDamage a target 1
      pure s
    _ -> ArnorRavaged <$> runMessage msg a
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
