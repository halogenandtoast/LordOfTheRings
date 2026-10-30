module LordOfTheRings.Enemy.Cards.OrcWarParty (orcWarParty) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OrcWarParty = OrcWarParty EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcWarParty :: EnemyCard OrcWarParty
orcWarParty = enemy OrcWarParty Cards.orcWarParty

-- | Cannot have attachments. While Orc War Party is in the staging area, enemies in the
-- staging area cannot take damage.
-- (The players cannot win the game while Orc War Party is in play: Stop the War Party,
-- the only stage that wins it, waits for the last one to leave play.)
instance HasModifiersFor OrcWarParty where
  getModifiersFor (OrcWarParty a) = do
    modifySelf a [CannotHaveAttachments]
    when (a.placement == StagingArea) do
      staged <- select EnemyInStagingArea
      for_ staged \e -> modified a e [CannotTakeDamage]

instance RunMessage OrcWarParty where
  runMessage msg (OrcWarParty attrs) = OrcWarParty <$> runMessage msg attrs
