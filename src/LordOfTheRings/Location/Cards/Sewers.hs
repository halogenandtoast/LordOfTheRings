module LordOfTheRings.Location.Cards.Sewers (sewers) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (placeUnderworldCards)

newtype Sewers = Sewers LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sewers :: LocationCard Sewers
sewers = location Sewers Cards.sewers

-- | Underworld 1.
-- When Revealed: Place the top card of the underworld deck facedown underneath the active
-- location, if able.
instance RunMessage Sewers where
  runMessage msg l@(Sewers a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      traverse_ (`placeUnderworldCards` 1) =<< select IsActiveLocation
      pure l
    _ -> Sewers <$> runMessage msg a
