module LordOfTheRings.Location.Cards.MinesOfGundabad (minesOfGundabad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype MinesOfGundabad = MinesOfGundabad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

minesOfGundabad :: LocationCard MinesOfGundabad
minesOfGundabad = location MinesOfGundabad Cards.minesOfGundabad

-- | Deep. Dagnir gets -10 engagement cost.
instance HasModifiersFor MinesOfGundabad where
  getModifiersFor (MinesOfGundabad a) = getDagnir >>= traverse_ \d -> modified a d [EngagementCostModifier (-10)]

-- | Shadow: If attacking enemy is Dagnir, the defending player must assign X damage among
-- characters he controls, where X is 1 more than the number of quest cards in the victory
-- display.
instance RunMessage MinesOfGundabad where
  runMessage msg l@(MinesOfGundabad a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      dagnir <- getDagnir
      when (dagnir == Just eid) $ dealDamageDivided a pid =<< dagnirX
      pure l
    _ -> MinesOfGundabad <$> runMessage msg a
