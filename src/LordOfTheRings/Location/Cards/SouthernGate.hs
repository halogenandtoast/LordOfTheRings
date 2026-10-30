module LordOfTheRings.Location.Cards.SouthernGate (southernGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Orc))

newtype SouthernGate = SouthernGate LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

southernGate :: LocationCard SouthernGate
southernGate = location SouthernGate Cards.southernGate

-- | Immune to player card effects.
-- While Southern Gate is the active location, each Orc enemy gets +2 attack.
instance HasModifiersFor SouthernGate where
  getModifiersFor (SouthernGate a) = do
    modified a a [ImmuneToPlayerCardEffects]
    when (a.placement == ActiveLocation) do
      orcs <- select (EnemyWithTrait Orc)
      for_ orcs \o -> modified a o [AttackModifier 2]

-- | Forced: After Southern Gate becomes the active location, each player with less than 35
-- threat must raise their threat to 35.
instance HasAbilities SouthernGate where
  getAbilities (SouthernGate a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage SouthernGate where
  runMessage msg l@(SouthernGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        threat <- (.threat) <$> getPlayer pid
        encounterRaisesThreat pid (35 - threat)
      pure l
    _ -> SouthernGate <$> runMessage msg a
