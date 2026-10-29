module LordOfTheRings.Objective.Cards.ChiefTurchTheAntleredCrown (chiefTurchTheAntleredCrown) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype ChiefTurchTheAntleredCrown = ChiefTurchTheAntleredCrown ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Chief Turch, as an ally, as he comes out.
chiefTurchTheAntleredCrown :: ObjectiveCard ChiefTurchTheAntleredCrown
chiefTurchTheAntleredCrown = objective ChiefTurchTheAntleredCrown Cards.chiefTurchTheAntleredCrown
