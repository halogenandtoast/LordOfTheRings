module LordOfTheRings.Location.Cards.RollingPlains (rollingPlains) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RollingPlains = RollingPlains LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rollingPlains :: LocationCard RollingPlains
rollingPlains = location RollingPlains Cards.rollingPlains

-- | When Revealed: Each copy of Rolling Plains gets +2 Threat until the end of the phase:
-- each in play, this one too.
-- Travel: Raise each player's threat by 2.
instance RunMessage RollingPlains where
  runMessage msg l@(RollingPlains a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      copies <- select (LocationWithCardCode a.locationCardCode)
      for_ copies \c -> phaseModifier a c (ThreatModifier 2)
      pure l
    TravelTo lid | lid == a.id -> do
      eachPlayer (`raiseThreat` 2)
      pure l
    _ -> RollingPlains <$> runMessage msg a
