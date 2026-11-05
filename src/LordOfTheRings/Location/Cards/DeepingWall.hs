module LordOfTheRings.Location.Cards.DeepingWall (deepingWall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype DeepingWall = DeepingWall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

deepingWall :: LocationCard DeepingWall
deepingWall = location DeepingWall Cards.deepingWall

-- | While Deeping Wall is the active location, each hero gets +1 defense.
instance HasModifiersFor DeepingWall where
  getModifiersFor (DeepingWall a) = when (a.placement == ActiveLocation) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [DefenseModifier 1]

-- | Forced: When Deeping Wall is explored, raise each player's threat by 4.
instance HasAbilities DeepingWall where
  getAbilities (DeepingWall a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage DeepingWall where
  runMessage msg l@(DeepingWall a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 4
      pure l
    _ -> DeepingWall <$> runMessage msg a
