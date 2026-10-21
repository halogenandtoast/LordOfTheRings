module LordOfTheRings.Ally.Cards.ErrandRider (errandRider) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype ErrandRider = ErrandRider AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

errandRider :: AllyCard ErrandRider
errandRider = ally ErrandRider Cards.errandRider

-- | Action: Exhaust Errand-rider to move 1 resource from the resource pool of a hero you
-- control to another hero's resource pool.
instance HasAbilities ErrandRider where
  getAbilities (ErrandRider a) =
    [ restricted a 1 (exists $ HeroControlledBy (PlayerWithId pid) <> HeroWithResources) $ ActionAbility (ExhaustCost $ toTarget a)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage ErrandRider where
  runMessage msg e@(ErrandRider a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      moveResourceFromHero pid a
      pure e
    _ -> ErrandRider <$> runMessage msg a
