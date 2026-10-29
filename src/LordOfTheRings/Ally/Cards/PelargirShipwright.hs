module LordOfTheRings.Ally.Cards.PelargirShipwright (pelargirShipwright) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype PelargirShipwright = PelargirShipwright AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelargirShipwright :: AllyCard PelargirShipwright
pelargirShipwright = ally PelargirShipwright Cards.pelargirShipwright

-- | Pelargir Shipwright gets +1 willpower for each hero you control with a printed spirit
-- resource icon.
instance HasModifiersFor PelargirShipwright where
  getModifiersFor (PelargirShipwright a) = for_ (placementController a.placement) \pid -> do
    n <- length <$> select (HeroControlledBy (PlayerWithId pid) <> HeroWithPrintedSphere Spirit)
    when (n > 0) $ modifySelf a [WillpowerModifier n]
