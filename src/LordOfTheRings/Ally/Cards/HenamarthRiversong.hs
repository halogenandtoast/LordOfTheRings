module LordOfTheRings.Ally.Cards.HenamarthRiversong (henamarthRiversong) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype HenamarthRiversong = HenamarthRiversong AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

henamarthRiversong :: AllyCard HenamarthRiversong
henamarthRiversong = ally HenamarthRiversong Cards.henamarthRiversong
