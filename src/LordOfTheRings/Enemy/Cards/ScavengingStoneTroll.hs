module LordOfTheRings.Enemy.Cards.ScavengingStoneTroll (scavengingStoneTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Safe))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype ScavengingStoneTroll = ScavengingStoneTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

scavengingStoneTroll :: EnemyCard ScavengingStoneTroll
scavengingStoneTroll = enemy ScavengingStoneTroll Cards.scavengingStoneTroll

-- | You cannot travel to a safe location while Scavenging Stone-troll has no damage on it.
-- While Scavenging Stone-troll is in the staging area, cards in each player's discard pile
-- cannot leave the discard pile.
instance HasModifiersFor ScavengingStoneTroll where
  getModifiersFor (ScavengingStoneTroll a) = do
    damage <- getDamage (toTarget a)
    when (damage == 0) do
      safe <- select (LocationWithKeyword Safe)
      for_ safe \l -> modified a l [CannotBeTraveledTo]
    when (a.placement == StagingArea) $ modified a GameTarget [DiscardPilesLocked]

instance RunMessage ScavengingStoneTroll where
  runMessage msg (ScavengingStoneTroll attrs) = ScavengingStoneTroll <$> runMessage msg attrs
