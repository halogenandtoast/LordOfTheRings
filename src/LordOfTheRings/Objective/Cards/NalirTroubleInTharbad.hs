module LordOfTheRings.Objective.Cards.NalirTroubleInTharbad (nalirTroubleInTharbad) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype NalirTroubleInTharbad = NalirTroubleInTharbad ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Nalir, as an ally, as he comes out.
nalirTroubleInTharbad :: ObjectiveCard NalirTroubleInTharbad
nalirTroubleInTharbad = objective NalirTroubleInTharbad Cards.nalirTroubleInTharbad
