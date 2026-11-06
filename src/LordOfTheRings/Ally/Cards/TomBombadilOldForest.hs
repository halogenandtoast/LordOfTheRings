module LordOfTheRings.Ally.Cards.TomBombadilOldForest (tomBombadilOldForest) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype TomBombadilOldForest = TomBombadilOldForest AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- Its round-end instructions are a lasting part of the resolved revelation, so
-- blanking Tom's text after it resolves does not cancel them.
tomBombadilOldForest :: AllyCard TomBombadilOldForest
tomBombadilOldForest = ally TomBombadilOldForest Cards.tomBombadilOldForest
