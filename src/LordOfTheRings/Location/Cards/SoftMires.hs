module LordOfTheRings.Location.Cards.SoftMires (softMires) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SoftMires = SoftMires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

softMires :: LocationCard SoftMires
softMires = location SoftMires Cards.softMires

-- | Mire 1.
-- Forced: When Soft Mires is discarded by the Mire keyword, the first player reveals the
-- top card of the encounter deck.
instance HasAbilities SoftMires where
  getAbilities (SoftMires a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

-- | Travel: Each player must raise his threat by 1 to travel here.
instance RunMessage SoftMires where
  runMessage msg l@(SoftMires a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \pid -> encounterRaisesThreat pid 1
      SoftMires <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      revealEncounterCard
      pure l
    _ -> SoftMires <$> runMessage msg a
