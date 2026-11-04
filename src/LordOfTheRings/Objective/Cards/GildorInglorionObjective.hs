module LordOfTheRings.Objective.Cards.GildorInglorionObjective (gildorInglorionObjective) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype GildorInglorionObjective = GildorInglorionObjective ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Gildor Inglorion, as an ally, as he comes out (see
-- the ally).
gildorInglorionObjective :: ObjectiveCard GildorInglorionObjective
gildorInglorionObjective = objective GildorInglorionObjective Cards.gildorInglorionObjective
