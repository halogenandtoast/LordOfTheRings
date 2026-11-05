module LordOfTheRings.Location.Cards.FoulSmellingCave (foulSmellingCave) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (shelob, placeResourceOnShelob)

newtype FoulSmellingCave = FoulSmellingCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

foulSmellingCave :: LocationCard FoulSmellingCave
foulSmellingCave = location FoulSmellingCave Cards.foulSmellingCave

instance HasAbilities FoulSmellingCave where
  getAbilities (FoulSmellingCave a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage FoulSmellingCave where
  runMessage msg l@(FoulSmellingCave a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResourceOnShelob a
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      isShelob <- selectAny (EnemyWithId eid <> shelob)
      enemyGetsAttack a eid (if isShelob then 2 else 1)
      pure l
    _ -> FoulSmellingCave <$> runMessage msg a
