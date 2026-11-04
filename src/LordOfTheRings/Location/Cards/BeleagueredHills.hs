module LordOfTheRings.Location.Cards.BeleagueredHills (beleagueredHills) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype BeleagueredHills = BeleagueredHills LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

beleagueredHills :: LocationCard BeleagueredHills
beleagueredHills = location BeleagueredHills Cards.beleagueredHills

-- | While Beleaguered Hills is the active location, reduce the total threat in the staging
-- area by X, where X is the total threat of each engaged enemy.
instance HasModifiersFor BeleagueredHills where
  getModifiersFor (BeleagueredHills a) = when (a.placement == ActiveLocation) do
    x <- sum <$> (traverse getEnemyThreat =<< select (EnemyEngagedWith AnyPlayer))
    when (x > 0) $ modified a GameTarget [StagingThreatBonus (negate x)]

-- | Forced: After Beleaguered Hills becomes the active location, each player engages a
-- non-unique enemy in the staging area.
instance HasAbilities BeleagueredHills where
  getAbilities (BeleagueredHills a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage BeleagueredHills where
  runMessage msg l@(BeleagueredHills a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        enemies <- select (EnemyInStagingArea <> not_ UniqueEnemy)
        chooseOrRunOneM p do
          questionLabeled "beleagueredHills.engage"
          causedBy a
          targets enemies (engageEnemy p)
      pure l
    _ -> BeleagueredHills <$> runMessage msg a
