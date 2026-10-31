module LordOfTheRings.Location.Cards.EasternMines (easternMines) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (chooseLostEffects, locateTest, triggerLostEffects)

newtype EasternMines = EasternMines LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easternMines :: LocationCard EasternMines
easternMines = location EasternMines Cards.easternMines

-- | Travel: The first player must make a locate test to travel here. If this test is
-- failed, return Eastern Mines to the staging area and trigger all "Lost:" effects in play.
-- Shadow: Defending player must choose and trigger 1 "Lost:" effect in play.
instance RunMessage EasternMines where
  runMessage msg l@(EasternMines a) = case msg of
    TravelTo lid | lid == a.id -> do
      locateTest a (placeLocation a.id StagingArea >> triggerLostEffects a) (pure ())
      EasternMines <$> runMessage msg a
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseLostEffects a [pid]
      pure l
    _ -> EasternMines <$> runMessage msg a
