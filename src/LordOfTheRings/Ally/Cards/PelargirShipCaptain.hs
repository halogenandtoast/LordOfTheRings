module LordOfTheRings.Ally.Cards.PelargirShipCaptain (pelargirShipCaptain) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype PelargirShipCaptain = PelargirShipCaptain AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelargirShipCaptain :: AllyCard PelargirShipCaptain
pelargirShipCaptain = ally PelargirShipCaptain Cards.pelargirShipCaptain

-- | Response: After Pelargir Ship Captain enters play, move 1 resource from the resource
-- pool of a hero you control to another hero's resource pool.
instance HasAbilities PelargirShipCaptain where
  getAbilities (PelargirShipCaptain a) =
    [ restricted a 1 (exists $ HeroControlledBy (PlayerWithId pid) <> HeroWithResources) $ response $ EntersPlayWindow After (toTarget a)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage PelargirShipCaptain where
  runMessage msg e@(PelargirShipCaptain a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      moveResourceFromHero pid a
      pure e
    _ -> PelargirShipCaptain <$> runMessage msg a
