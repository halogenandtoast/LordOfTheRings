module LordOfTheRings.Location.Cards.RuinsOfOsgiliath (ruinsOfOsgiliath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)

newtype RuinsOfOsgiliath = RuinsOfOsgiliath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinsOfOsgiliath :: LocationCard RuinsOfOsgiliath
ruinsOfOsgiliath = location RuinsOfOsgiliath Cards.ruinsOfOsgiliath

instance HasModifiersFor RuinsOfOsgiliath where
  getModifiersFor (RuinsOfOsgiliath a) = do
    crossed <- crossedAnduin
    unless crossed $ modifySelf a [ThreatModifier 3]
