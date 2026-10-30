module LordOfTheRings.Objective.Cards.DaechanarsBrand (daechanarsBrand) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (clueAbilities, runClue)
import LordOfTheRings.Trait (Trait (Undead))

newtype DaechanarsBrand = DaechanarsBrand ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

daechanarsBrand :: ObjectiveCard DaechanarsBrand
daechanarsBrand = objective DaechanarsBrand Cards.daechanarsBrand

-- | Action: Exhaust Amarthiúl or a hero to claim Daechanar's Brand. Then, attach Daechanar's
-- Brand to that character.
instance HasAbilities DaechanarsBrand where
  getAbilities (DaechanarsBrand a) = clueAbilities a

-- | Attached character gets +1 attack while attacking Undead enemies.
instance HasModifiersFor DaechanarsBrand where
  getModifiersFor (DaechanarsBrand a) = withHost a \host -> modified a host [AttackBonusAgainst Undead 1]

instance RunMessage DaechanarsBrand where
  runMessage msg (DaechanarsBrand attrs) = DaechanarsBrand <$> runClue msg attrs
