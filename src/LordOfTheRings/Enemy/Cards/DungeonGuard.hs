module LordOfTheRings.Enemy.Cards.DungeonGuard (dungeonGuard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DungeonGuard = DungeonGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 1. Response: After the captured card(s) underneath Dungeon Guard are rescued,
-- the players may put 1 of them into play at no cost. It has left play by then, so the
-- scenario offers the response as it rescues them.
dungeonGuard :: EnemyCard DungeonGuard
dungeonGuard = enemy DungeonGuard Cards.dungeonGuard

instance RunMessage DungeonGuard where
  runMessage msg (DungeonGuard attrs) = DungeonGuard <$> runMessage msg attrs
