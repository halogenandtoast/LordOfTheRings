module LordOfTheRings.Ally.Cards.DreamChaser (dreamChaser) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype DreamChaser = DreamChaser AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Ship objective, under the control of the player who took it as the fleet was
-- prepared.
dreamChaser :: AllyCard DreamChaser
dreamChaser = ally DreamChaser Cards.dreamChaser

-- | Immune to player card effects. The Dream-chaser can always be committed to Sailing
-- tests (even if you are not the first player), and counts as 2 characters when
-- committed to a Sailing test.
instance HasModifiersFor DreamChaser where
  getModifiersFor (DreamChaser a) = do
    shipModifiers a
    modified a a [CommittableToAnySailingTest, CountsAsSailingCharacters 2]

-- | If the Dream-chaser is destroyed, the players lose the game.
instance RunMessage DreamChaser where
  runMessage msg (DreamChaser a) = DreamChaser <$> runShip (gameOver Lost) msg a
