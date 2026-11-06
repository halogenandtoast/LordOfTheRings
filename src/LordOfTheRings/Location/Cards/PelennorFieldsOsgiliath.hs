module LordOfTheRings.Location.Cards.PelennorFieldsOsgiliath (pelennorFieldsOsgiliath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)

newtype PelennorFieldsOsgiliath = PelennorFieldsOsgiliath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelennorFieldsOsgiliath :: LocationCard PelennorFieldsOsgiliath
pelennorFieldsOsgiliath = location PelennorFieldsOsgiliath Cards.pelennorFieldsOsgiliath

instance HasModifiersFor PelennorFieldsOsgiliath where
  getModifiersFor (PelennorFieldsOsgiliath a) = when (a.placement == StagingArea) do
    crossed <- crossedAnduin
    when crossed $ modifySelf a [ThreatPenaltyForNotTravelingTo 3]
