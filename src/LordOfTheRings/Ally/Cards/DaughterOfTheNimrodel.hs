module LordOfTheRings.Ally.Cards.DaughterOfTheNimrodel (daughterOfTheNimrodel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DaughterOfTheNimrodel = DaughterOfTheNimrodel AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

daughterOfTheNimrodel :: AllyCard DaughterOfTheNimrodel
daughterOfTheNimrodel = ally DaughterOfTheNimrodel Cards.daughterOfTheNimrodel
