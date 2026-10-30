module LordOfTheRings.Location.Cards.WaterLoggedHalls (waterLoggedHalls) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Undead))

-- | Grotto side; its Underwater side is Sunken Temple.
newtype WaterLoggedHalls = WaterLoggedHalls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

waterLoggedHalls :: LocationCard WaterLoggedHalls
waterLoggedHalls = location WaterLoggedHalls Cards.waterLoggedHalls

-- | While Water-logged Halls is the active location, each Undead enemy in play gets -1
-- threat.
instance HasModifiersFor WaterLoggedHalls where
  getModifiersFor (WaterLoggedHalls a) = when (a.placement == ActiveLocation) do
    undead <- select (EnemyWithTrait Undead)
    for_ undead \e -> modified a e [ThreatModifier (-1)]

-- | Response: After you travel to Water-logged Halls, you may flip it to its Underwater
-- side.
instance HasAbilities WaterLoggedHalls where
  getAbilities (WaterLoggedHalls a) = [mkAbility a 1 $ response $ TraveledToWindow After (LocationWithId a.id)]

instance RunMessage WaterLoggedHalls where
  runMessage msg l@(WaterLoggedHalls a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure l
    _ -> WaterLoggedHalls <$> runMessage msg a
