module LordOfTheRings.Enemy.Cards.MoriaBats (moriaBats) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype MoriaBats = MoriaBats EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

moriaBats :: EnemyCard MoriaBats
moriaBats = enemy MoriaBats Cards.moriaBats

-- | Only characters with ranged can attack or defend against Moria Bats.
-- While Moria Bats is engaged with a player, it gets +1 attack for each other enemy engaged
-- with that player.
instance HasModifiersFor MoriaBats where
  getModifiersFor (MoriaBats a) = do
    others <- case a.placement of
      EngagedWith pid -> length <$> select (EnemyEngagedWith (PlayerWithId pid) <> NotEnemy (EnemyWithId a.id))
      _ -> pure 0
    modified a a $ OnlyFoughtByKeyword Ranged : [AttackModifier others | others > 0]
