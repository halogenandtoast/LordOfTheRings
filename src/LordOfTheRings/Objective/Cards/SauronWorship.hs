module LordOfTheRings.Objective.Cards.SauronWorship (sauronWorship) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype SauronWorship = SauronWorship ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sauron Worship enters play with 1 damage token on it.
sauronWorship :: ObjectiveCard SauronWorship
sauronWorship = objective (\a -> SauronWorship a {objectiveTokens = addTokens Damage 1 mempty}) Cards.sauronWorship

damage :: ObjectiveAttrs -> Int
damage = countTokens Damage . objectiveTokens

-- | Each player's threat elimination level is reduced by 1 for each damage token here.
instance HasModifiersFor SauronWorship where
  getModifiersFor (SauronWorship a) = when (damage a > 0) do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [EliminationThreatModifier (negate $ damage a)]

-- | Forced: At the end of the round, place 1 damage token here.
instance HasAbilities SauronWorship where
  getAbilities (SauronWorship a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage SauronWorship where
  runMessage msg o@(SauronWorship attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeTokens attrs attrs Damage 1
      pure o
    _ -> SauronWorship <$> runMessage msg attrs
