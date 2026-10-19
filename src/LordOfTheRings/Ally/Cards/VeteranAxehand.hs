module LordOfTheRings.Ally.Cards.VeteranAxehand (veteranAxehand) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype VeteranAxehand = VeteranAxehand AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

veteranAxehand :: AllyCard VeteranAxehand
veteranAxehand = ally VeteranAxehand Cards.veteranAxehand
