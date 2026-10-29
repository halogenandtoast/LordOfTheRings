module LordOfTheRings.Objective.Cards.NalirTheNinInEilph (nalirTheNinInEilph) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype NalirTheNinInEilph = NalirTheNinInEilph ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Nalir, as an ally, as he comes out.
nalirTheNinInEilph :: ObjectiveCard NalirTheNinInEilph
nalirTheNinInEilph = objective NalirTheNinInEilph Cards.nalirTheNinInEilph
