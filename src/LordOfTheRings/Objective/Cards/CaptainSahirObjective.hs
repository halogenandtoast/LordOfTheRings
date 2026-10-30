module LordOfTheRings.Objective.Cards.CaptainSahirObjective (captainSahirObjective) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype CaptainSahirObjective = CaptainSahirObjective ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Captain Sahír, objective-ally side faceup, as he comes out.
captainSahirObjective :: ObjectiveCard CaptainSahirObjective
captainSahirObjective = objective CaptainSahirObjective Cards.captainSahirObjective
