module LordOfTheRings.Enemy.Cards.SwarmsOfMosquitos (swarmsOfMosquitos) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SwarmsOfMosquitos = SwarmsOfMosquitos EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

swarmsOfMosquitos :: EnemyCard SwarmsOfMosquitos
swarmsOfMosquitos = enemy SwarmsOfMosquitos Cards.swarmsOfMosquitos

-- | Cannot have attachments.
-- While Swarms of Mosquitoes is in the victory display, it gains: "Forced: After the
-- players advance to a quest stage, return Swarms of Mosquitoes to the staging area." The
-- scenario returns it, as a card in the victory display is not in play.
instance HasModifiersFor SwarmsOfMosquitos where
  getModifiersFor (SwarmsOfMosquitos a) = modifySelf a [CannotHaveAttachments]

instance RunMessage SwarmsOfMosquitos where
  runMessage msg (SwarmsOfMosquitos attrs) = SwarmsOfMosquitos <$> runMessage msg attrs
