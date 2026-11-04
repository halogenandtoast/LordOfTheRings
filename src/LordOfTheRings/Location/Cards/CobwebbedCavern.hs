module LordOfTheRings.Location.Cards.CobwebbedCavern (cobwebbedCavern) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (isOnTrack)

newtype CobwebbedCavern = CobwebbedCavern LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Track 2.
cobwebbedCavern :: LocationCard CobwebbedCavern
cobwebbedCavern = location CobwebbedCavern Cards.cobwebbedCavern

-- | While On Track is in play, Cobwebbed Cavern gets +2 threat.
instance HasModifiersFor CobwebbedCavern where
  getModifiersFor (CobwebbedCavern a) = whenM isOnTrack $ modifySelf a [ThreatModifier 2]

-- | Shadow: Exhaust a character you control.
instance RunMessage CobwebbedCavern where
  runMessage msg l@(CobwebbedCavern attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure l
    _ -> CobwebbedCavern <$> runMessage msg attrs
