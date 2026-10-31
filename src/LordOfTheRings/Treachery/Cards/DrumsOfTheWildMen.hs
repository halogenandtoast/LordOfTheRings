module LordOfTheRings.Treachery.Cards.DrumsOfTheWildMen (drumsOfTheWildMen) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheDruadanForest.Helpers (prowlValue)
import LordOfTheRings.Trait (Trait (Wose))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DrumsOfTheWildMen = DrumsOfTheWildMen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drumsOfTheWildMen :: TreacheryCard DrumsOfTheWildMen
drumsOfTheWildMen = treachery DrumsOfTheWildMen Cards.drumsOfTheWildMen

-- | When Revealed: Each player must trigger the prowl keyword on each enemy engaged with
-- him, as if that enemy had just been revealed from the encounter deck. Then, return each
-- engaged Wose enemy to the staging area. If no enemies are returned to the staging area by
-- this effect, Drums of the Wild Men gains surge.
instance RunMessage DrumsOfTheWildMen where
  runMessage msg t@(DrumsOfTheWildMen attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
        prowl <- sum <$> traverse prowlValue engaged
        when (prowl > 0) $ discardGroupResources pid prowl
      woses <- select (EnemyWithTrait Wose <> EnemyEngagedWith AnyPlayer)
      for_ woses \w -> placeEnemy w StagingArea
      when (null woses) revealEncounterCard
      pure t
    _ -> DrumsOfTheWildMen <$> runMessage msg attrs
