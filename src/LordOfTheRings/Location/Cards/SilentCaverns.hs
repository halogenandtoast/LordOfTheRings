module LordOfTheRings.Location.Cards.SilentCaverns (silentCaverns) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SilentCaverns = SilentCaverns LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

silentCaverns :: LocationCard SilentCaverns
silentCaverns = location SilentCaverns Cards.silentCaverns

-- | Lost: Exhaust all characters.
instance RunMessage SilentCaverns where
  runMessage msg l@(SilentCaverns attrs) = case msg of
    TriggerLostEffects _ -> do
      traverse_ exhaust =<< selectCharacters ReadyHero ReadyAlly
      pure l
    _ -> SilentCaverns <$> runMessage msg attrs
