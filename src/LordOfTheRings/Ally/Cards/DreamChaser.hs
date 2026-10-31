module LordOfTheRings.Ally.Cards.DreamChaser (dreamChaser, dreamChaserUpgraded) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype DreamChaser = DreamChaser AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Ship objective, under the control of the player who took it as the fleet was
-- prepared.
dreamChaser :: AllyCard DreamChaser
dreamChaser = ally DreamChaser Cards.dreamChaser

-- | Its Upgraded side (the Dream-chaser campaign): it counts as 3 characters.
dreamChaserUpgraded :: AllyCard DreamChaser
dreamChaserUpgraded = ally DreamChaser Cards.dreamChaserUpgraded

-- | Immune to player card effects. The Dream-chaser can always be committed to Sailing
-- tests (even if you are not the first player), and counts as 2 characters when
-- committed to a Sailing test.
instance HasModifiersFor DreamChaser where
  getModifiersFor (DreamChaser a) = do
    shipModifiers a
    modified a a [CommittableToAnySailingTest, CountsAsSailingCharacters $ if toCardCode a == toCardCode Cards.dreamChaserUpgraded then 3 else 2]

-- | If the Dream-chaser is destroyed, the players lose the game.
instance RunMessage DreamChaser where
  runMessage msg (DreamChaser a) = DreamChaser <$> runShip (gameOver Lost) msg a
