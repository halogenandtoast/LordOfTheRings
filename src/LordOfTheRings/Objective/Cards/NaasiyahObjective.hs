module LordOfTheRings.Objective.Cards.NaasiyahObjective (naasiyahObjective) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype NaasiyahObjective = NaasiyahObjective ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The last player gains control of Na'asiyah, objective-ally side faceup, as she comes out.
naasiyahObjective :: ObjectiveCard NaasiyahObjective
naasiyahObjective = objective NaasiyahObjective Cards.naasiyahObjective
