module LordOfTheRings.Location.Cards.SilentCaverns (silentCaverns) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (lostTriggered)

newtype SilentCaverns = SilentCaverns LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

silentCaverns :: LocationCard SilentCaverns
silentCaverns = location SilentCaverns Cards.silentCaverns

-- | Lost: Exhaust all characters.
instance RunMessage SilentCaverns where
  runMessage msg l@(SilentCaverns attrs) = case msg of
    _ | lostTriggered attrs msg -> do
      traverse_ exhaust =<< selectCharacters ReadyHero ReadyAlly
      pure l
    _ -> SilentCaverns <$> runMessage msg attrs
