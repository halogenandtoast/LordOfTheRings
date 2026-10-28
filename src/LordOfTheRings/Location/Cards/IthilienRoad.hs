module LordOfTheRings.Location.Cards.IthilienRoad (ithilienRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype IthilienRoad = IthilienRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ithilienRoad :: LocationCard IthilienRoad
ithilienRoad = location IthilienRoad Cards.ithilienRoad

-- | While Ithilien Road is the active location, the engagement cost of each enemy in the
-- staging area is 0.
instance HasModifiersFor IthilienRoad where
  getModifiersFor (IthilienRoad a) = when (a.placement == ActiveLocation) do
    enemies <- select EnemyInStagingArea
    for_ enemies \eid -> do
      cost <- (.engagementCost) <$> getEnemy eid
      modified a eid [EngagementCostModifier (negate cost)]

-- | Shadow: Return attacking enemy to the staging area after it attacks.
instance RunMessage IthilienRoad where
  runMessage msg l@(IthilienRoad a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> IthilienRoad <$> runMessage msg a
