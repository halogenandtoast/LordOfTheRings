module LordOfTheRings.Objective.Cards.GandalfsDelay (gandalfsDelay) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype GandalfsDelay = GandalfsDelay ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the A Shadow of the Past set. Setup: Add Gandalf's Delay to the staging
-- area, which the scenario setting it up does, before the starting hands are drawn.
gandalfsDelay :: ObjectiveCard GandalfsDelay
gandalfsDelay = objective GandalfsDelay Cards.gandalfsDelay

-- | Each player draws 1 less card in his starting hand.
instance HasModifiersFor GandalfsDelay where
  getModifiersFor (GandalfsDelay a) = modified a GameTarget [StartingHandSize (-1)]

instance RunMessage GandalfsDelay where
  runMessage msg (GandalfsDelay a) = GandalfsDelay <$> runMessage msg a
