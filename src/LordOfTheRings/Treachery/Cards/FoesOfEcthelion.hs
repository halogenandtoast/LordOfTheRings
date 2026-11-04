module LordOfTheRings.Treachery.Cards.FoesOfEcthelion (foesOfEcthelion) where

import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Trait (Trait (Raider))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FoesOfEcthelion = FoesOfEcthelion TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

foesOfEcthelion :: TreacheryCard FoesOfEcthelion
foesOfEcthelion = treachery FoesOfEcthelion Cards.foesOfEcthelion

-- | When Revealed: Add 2 resources to a Raider enemy in the staging area. Each Raider enemy
-- in the staging area gets +1 threat for each resource on it until the end of the phase.
-- If there are no Raider enemies in the staging area, Foes of Ecthelion gains surge.
instance RunMessage FoesOfEcthelion where
  runMessage msg t@(FoesOfEcthelion attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      raiders <- select (EnemyWithTrait Raider <> EnemyInStagingArea)
      if null raiders
        then revealEncounterCard
        else do
          lead <- getLead
          chooseOrRunOneM lead do
            questionLabeled "foesOfEcthelion.raider"
            causedBy attrs
            targets raiders \r -> placeResources attrs r 2
          doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      raiders <- select (EnemyWithTrait Raider <> EnemyInStagingArea)
      for_ raiders \r -> do
        n <- countTokens Resource . enemyTokens . toAttrs <$> getEnemy r
        when (n > 0) $ phaseModifier attrs r (ThreatModifier n)
      pure t
    _ -> FoesOfEcthelion <$> runMessage msg attrs
