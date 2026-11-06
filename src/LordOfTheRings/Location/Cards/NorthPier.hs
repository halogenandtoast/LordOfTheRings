module LordOfTheRings.Location.Cards.NorthPier (northPier) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies)

newtype NorthPier = NorthPier LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

northPier :: LocationCard NorthPier
northPier = location NorthPier Cards.northPier

instance HasModifiersFor NorthPier where
  getModifiersFor (NorthPier a) = do
    enemies <- smaugEnemies
    for_ enemies \eid -> modified a eid [CannotTakeDamage]
