module LordOfTheRings.Objective.Cards.PoisonedCounsels (poisonedCounsels) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype PoisonedCounsels = PoisonedCounsels ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

poisonedCounsels :: ObjectiveCard PoisonedCounsels
poisonedCounsels = objective PoisonedCounsels Cards.poisonedCounsels

-- | At the end of each round, raise each player's threat by 2.
instance HasAbilities PoisonedCounsels where
  getAbilities (PoisonedCounsels a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage PoisonedCounsels where
  runMessage msg o@(PoisonedCounsels attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 2
      pure o
    _ -> PoisonedCounsels <$> runMessage msg attrs
