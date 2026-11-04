module LordOfTheRings.Location.Cards.CityWall (cityWall) where

import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (getMinasTirith)

newtype CityWall = CityWall LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cityWall :: LocationCard CityWall
cityWall = location CityWall Cards.cityWall

-- | While City Wall is the active location, damage that would be placed on Minas Tirith
-- must be placed here first: as much as it takes before it is discarded, the rest going
-- on to Minas Tirith.
instance HasModifiersFor CityWall where
  getModifiersFor (CityWall a) = when (a.placement == ActiveLocation) do
    room <- subtract a.damage <$> getLocationQuestPoints a.id
    getMinasTirith >>= traverse_ \o -> modified a (ObjectiveTarget o) [DamagePlacedFirstOn (toTarget a) room]

-- | Forced: When City Wall has damage equal to its quest points, discard it. Then, each
-- player raises his threat by 3.
instance RunMessage CityWall where
  runMessage msg (CityWall a) = case msg of
    CheckWindows ws | any (\case Window After (DealtDamage t _) -> t == toTarget a; _ -> False) ws -> do
      qp <- getLocationQuestPoints a.id
      when (a.damage >= qp) do
        discard a
        eachPlayer \p -> encounterRaisesThreat p 3
      CityWall <$> runMessage msg a
    _ -> CityWall <$> runMessage msg a
