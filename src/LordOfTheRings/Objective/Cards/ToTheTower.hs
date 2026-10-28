module LordOfTheRings.Objective.Cards.ToTheTower (toTheTower) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype ToTheTower = ToTheTower ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

toTheTower :: ObjectiveCard ToTheTower
toTheTower = objective ToTheTower Cards.toTheTower

-- | To the Tower cannot leave the staging area: no card moves it, so nothing guards it.
-- Remove Faramir from the game: the scenario takes him from each deck at setup.
-- If 10 or more progress tokens are on To the Tower, the players have lost the game.
instance RunMessage ToTheTower where
  runMessage msg (ToTheTower attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      attrs' <- runMessage msg attrs
      when (countTokens Progress (objectiveTokens attrs') >= 10) $ gameOver Lost
      pure $ ToTheTower attrs'
    _ -> ToTheTower <$> runMessage msg attrs
