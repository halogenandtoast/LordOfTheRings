module LordOfTheRings.Enemy.Cards.ReanimatedDead (reanimatedDead) where

import LordOfTheRings.Card.Synthetic qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

-- | A player card fighting face down as an enemy, with no text of its own. Leaving play,
-- it goes to its owner's discard pile, as the game keeps it with the Orc Guards.
newtype ReanimatedDead = ReanimatedDead EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

reanimatedDead :: EnemyCard ReanimatedDead
reanimatedDead = enemy ReanimatedDead Cards.reanimatedDead

instance RunMessage ReanimatedDead where
  runMessage msg (ReanimatedDead attrs) = ReanimatedDead <$> runMessage msg attrs
