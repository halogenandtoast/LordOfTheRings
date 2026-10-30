module LordOfTheRings.Ally.Cards.KnightOfTheWhiteTower (knightOfTheWhiteTower) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype KnightOfTheWhiteTower = KnightOfTheWhiteTower AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Knight of the White Tower's resource cost must be paid from a single hero's resource
-- pool, as its card def says.
knightOfTheWhiteTower :: AllyCard KnightOfTheWhiteTower
knightOfTheWhiteTower = ally KnightOfTheWhiteTower Cards.knightOfTheWhiteTower

instance RunMessage KnightOfTheWhiteTower where
  runMessage msg (KnightOfTheWhiteTower a) = KnightOfTheWhiteTower <$> runMessage msg a
