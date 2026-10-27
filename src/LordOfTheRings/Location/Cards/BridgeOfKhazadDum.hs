module LordOfTheRings.Location.Cards.BridgeOfKhazadDum (bridgeOfKhazadDum) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype BridgeOfKhazadDum = BridgeOfKhazadDum LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

bridgeOfKhazadDum :: LocationCard BridgeOfKhazadDum
bridgeOfKhazadDum = location BridgeOfKhazadDum Cards.bridgeOfKhazadDum

-- | While Bridge of Khazad-dûm is the active location, players cannot play cards.
instance HasModifiersFor BridgeOfKhazadDum where
  getModifiersFor (BridgeOfKhazadDum a) =
    when (a.placement == ActiveLocation) $ modified a GameTarget [CannotPlay AnyCard]
