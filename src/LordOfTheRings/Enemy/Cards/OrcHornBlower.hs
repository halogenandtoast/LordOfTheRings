module LordOfTheRings.Enemy.Cards.OrcHornBlower (orcHornBlower) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcHornBlower = OrcHornBlower EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcHornBlower :: EnemyCard OrcHornBlower
orcHornBlower = enemy OrcHornBlower Cards.orcHornBlower

-- | Surge. When Revealed: Reveal 1 card from the encounter deck and add it to the staging
-- area.
instance RunMessage OrcHornBlower where
  runMessage msg e@(OrcHornBlower attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealEncounterCard
      pure e
    _ -> OrcHornBlower <$> runMessage msg attrs
