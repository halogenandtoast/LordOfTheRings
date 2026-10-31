module LordOfTheRings.Enemy.Cards.OverseerMaurul (overseerMaurul) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (mostGoblinResources)

newtype OverseerMaurul = OverseerMaurul EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

overseerMaurul :: EnemyCard OverseerMaurul
overseerMaurul = enemy OverseerMaurul Cards.overseerMaurul

-- | Overseer Maurûl gets +X threat, +X attack and +X defense, where X is the highest
-- number of resources on a Goblin enemy in play.
-- The players cannot win the game unless Overseer Maurûl is in the victory display: The
-- Fate of Balin holds that.
instance HasModifiersFor OverseerMaurul where
  getModifiersFor (OverseerMaurul a) = do
    x <- mostGoblinResources
    when (x > 0) $ modifySelf a [ThreatModifier x, AttackModifier x, DefenseModifier x]

instance RunMessage OverseerMaurul where
  runMessage msg (OverseerMaurul attrs) = OverseerMaurul <$> runMessage msg attrs
