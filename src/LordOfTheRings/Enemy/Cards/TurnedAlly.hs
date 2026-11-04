module LordOfTheRings.Enemy.Cards.TurnedAlly (turnedAlly) where

import LordOfTheRings.Card (CardCode, toCardDef)
import LordOfTheRings.Enemy.Import.Lifted

-- | An ally that lost the ally card type and gained the enemy card type, as Secret Cultist
-- turns it: its text box is blank, so it has no text of its own; its stats are its card's.
newtype TurnedAlly = TurnedAlly EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

turnedAlly :: CardCode -> EnemyCard TurnedAlly
turnedAlly = enemy TurnedAlly . toCardDef

instance RunMessage TurnedAlly where
  runMessage msg (TurnedAlly attrs) = TurnedAlly <$> runMessage msg attrs
