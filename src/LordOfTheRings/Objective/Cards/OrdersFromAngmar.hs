module LordOfTheRings.Objective.Cards.OrdersFromAngmar (ordersFromAngmar) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (clueAbilities, runClue)
import LordOfTheRings.Trait (Trait (Undead))

newtype OrdersFromAngmar = OrdersFromAngmar ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

ordersFromAngmar :: ObjectiveCard OrdersFromAngmar
ordersFromAngmar = objective OrdersFromAngmar Cards.ordersFromAngmar

-- | Action: Exhaust Amarthiúl or a hero to claim Orders from Angmar. Then, attach Orders
-- from Angmar to that character.
instance HasAbilities OrdersFromAngmar where
  getAbilities (OrdersFromAngmar a) = clueAbilities a

-- | Attached character gets +1 defense while defending against Undead enemies.
instance HasModifiersFor OrdersFromAngmar where
  getModifiersFor (OrdersFromAngmar a) = withHost a \host -> modified a host [DefenseBonusAgainst Undead 1]

instance RunMessage OrdersFromAngmar where
  runMessage msg (OrdersFromAngmar attrs) = OrdersFromAngmar <$> runClue msg attrs
