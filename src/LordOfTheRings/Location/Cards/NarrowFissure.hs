module LordOfTheRings.Location.Cards.NarrowFissure (narrowFissure) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (readyCaveTorch)
import LordOfTheRings.Trait (Trait (Dark))

newtype NarrowFissure = NarrowFissure LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

narrowFissure :: LocationCard NarrowFissure
narrowFissure = location NarrowFissure Cards.narrowFissure

-- | Each Dark location in play gets +2 quest points.
-- While Cave Torch is ready, progress cannot be placed on Narrow Fissure.
instance HasModifiersFor NarrowFissure where
  getModifiersFor (NarrowFissure a) = do
    dark <- select (LocationWithTrait Dark)
    for_ dark \l -> modified a l [QuestPointModifier 2]
    torch <- readyCaveTorch
    when (isJust torch) $ modified a a [ProgressLimit 0]

instance RunMessage NarrowFissure where
  runMessage msg (NarrowFissure a) = NarrowFissure <$> runMessage msg a
