module LordOfTheRings.Location.Cards.ImpassableBog (impassableBog) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)

newtype ImpassableBog = ImpassableBog LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

impassableBog :: LocationCard ImpassableBog
impassableBog = location ImpassableBog Cards.impassableBog

-- | When Revealed: Place 1 resource token on Gollum for each location card in the staging
-- area.
instance RunMessage ImpassableBog where
  runMessage msg l@(ImpassableBog a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      locations <- selectCount LocationInStagingArea
      when (locations > 0) $ placeOnGollum a locations
      pure l
    _ -> ImpassableBog <$> runMessage msg a
