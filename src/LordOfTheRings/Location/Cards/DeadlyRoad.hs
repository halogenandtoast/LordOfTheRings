module LordOfTheRings.Location.Cards.DeadlyRoad (deadlyRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype DeadlyRoad = DeadlyRoad LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

deadlyRoad :: LocationCard DeadlyRoad
deadlyRoad = location DeadlyRoad Cards.deadlyRoad

-- | Its travel cost cannot be paid without a Fellowship resource.
instance HasModifiersFor DeadlyRoad where
  getModifiersFor (DeadlyRoad a) = do
    fellowship <- getGroupResourcesOfSphere Fellowship
    when (fellowship < 1) $ modifySelf a [CannotBeTraveledTo]

-- | While Deadly Road is in the staging area, it gains: "Forced: After a 'when revealed'
-- effect is canceled, raise each player's threat by 1."
instance HasAbilities DeadlyRoad where
  getAbilities (DeadlyRoad a) = [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After | a.placement == StagingArea]

-- | Travel: Spend 1 Fellowship resource to travel here.
instance RunMessage DeadlyRoad where
  runMessage msg l@(DeadlyRoad a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      spendGroupResources lead Fellowship 1
      DeadlyRoad <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure l
    _ -> DeadlyRoad <$> runMessage msg a
