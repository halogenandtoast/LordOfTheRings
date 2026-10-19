module LordOfTheRings.Location.Cards.NecromancersPass (necromancersPass) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype NecromancersPass = NecromancersPass LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

necromancersPass :: LocationCard NecromancersPass
necromancersPass = location NecromancersPass Cards.necromancersPass
