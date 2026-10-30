module LordOfTheRings.Objective.Cards.SavageCounterattack (savageCounterattack) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype SavageCounterattack = SavageCounterattack ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

savageCounterattack :: ObjectiveCard SavageCounterattack
savageCounterattack = objective SavageCounterattack Cards.savageCounterattack

-- | Forced: After Savage Counter-attack enters play, advance to stage 2A (Hunting the Orcs
-- advances as it flips into this).
-- Forced: After an enemy attacks and destroys a character, discard 1 resource token here.
instance HasAbilities SavageCounterattack where
  getAbilities (SavageCounterattack a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After Nothing]

-- | If there are no resource tokens here, the players lose the game.
instance RunMessage SavageCounterattack where
  runMessage msg o@(SavageCounterattack attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTokens attrs attrs Resource 1
      pure o
    RemoveTokens _ (isTarget attrs -> True) Resource _ -> do
      attrs' <- runMessage msg attrs
      when (countTokens Resource (objectiveTokens attrs') == 0) $ gameOver Lost
      pure $ SavageCounterattack attrs'
    _ -> SavageCounterattack <$> runMessage msg attrs
