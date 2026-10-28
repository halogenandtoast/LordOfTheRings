module LordOfTheRings.Location.Cards.CityStreet (cityStreet) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CityStreet = CityStreet LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cityStreet :: LocationCard CityStreet
cityStreet = location CityStreet Cards.cityStreet

-- | Surge.
-- While City Street is in the staging area, players cannot travel to a location that does
-- not have the title City Street.
instance HasModifiersFor CityStreet where
  getModifiersFor (CityStreet a) = when (a.placement == StagingArea) do
    others <- traverse getLocation =<< select LocationInStagingArea
    for_ [l | l <- others, toCardCode l /= toCardCode Cards.cityStreet] \l -> modified a l [CannotBeTraveledTo]

-- | Shadow: attacking enemy gets +2 attack.
instance RunMessage CityStreet where
  runMessage msg l@(CityStreet a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 2
      pure l
    _ -> CityStreet <$> runMessage msg a
