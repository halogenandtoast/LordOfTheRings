module LordOfTheRings.Ally.Cards.GwaihirTheBlackGateOpens (gwaihirTheBlackGateOpens) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype GwaihirTheBlackGateOpens = GwaihirTheBlackGateOpens AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally Gwaihir, a boon, once the first player has taken control of him.
-- Ranged. Sentinel.
gwaihirTheBlackGateOpens :: AllyCard GwaihirTheBlackGateOpens
gwaihirTheBlackGateOpens = ally GwaihirTheBlackGateOpens Cards.gwaihirTheBlackGateOpens

instance RunMessage GwaihirTheBlackGateOpens where
  runMessage msg (GwaihirTheBlackGateOpens a) = GwaihirTheBlackGateOpens <$> runMessage msg a
