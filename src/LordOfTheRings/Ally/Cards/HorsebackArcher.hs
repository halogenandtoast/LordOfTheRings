module LordOfTheRings.Ally.Cards.HorsebackArcher (horsebackArcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype HorsebackArcher = HorsebackArcher AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

horsebackArcher :: AllyCard HorsebackArcher
horsebackArcher = ally HorsebackArcher Cards.horsebackArcher
