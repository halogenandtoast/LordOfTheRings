module LordOfTheRings.Ally.Cards.MinerOfTheIronHills (minerOfTheIronHills) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype MinerOfTheIronHills = MinerOfTheIronHills AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

minerOfTheIronHills :: AllyCard MinerOfTheIronHills
minerOfTheIronHills = ally MinerOfTheIronHills Cards.minerOfTheIronHills
