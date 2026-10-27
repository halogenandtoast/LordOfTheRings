module LordOfTheRings.Location.Cards.TurbulentWaters (turbulentWaters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TurbulentWaters = TurbulentWaters LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

turbulentWaters :: LocationCard TurbulentWaters
turbulentWaters = location TurbulentWaters Cards.turbulentWaters

-- | While Turbulent Waters is the active location, players cannot optionally engage enemies.
instance HasModifiersFor TurbulentWaters where
  getModifiersFor (TurbulentWaters a) =
    when (a.placement == ActiveLocation) $ modified a GameTarget [CannotBeOptionallyEngagedBy AnyPlayer]
