module LordOfTheRings.Enemy.Cards.DungeonWarg (dungeonWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DungeonWarg = DungeonWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, as its card def says.
dungeonWarg :: EnemyCard DungeonWarg
dungeonWarg = enemy DungeonWarg Cards.dungeonWarg

-- | When Revealed: Dungeon Warg makes an immediate attack against the player who controls
-- the most allies, the first player choosing among those tied.
-- Shadow: Attacking enemy gets +1 attack.
instance RunMessage DungeonWarg where
  runMessage msg e@(DungeonWarg attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      counts <- for players \p -> (p,) . length <$> select (AllyControlledBy $ PlayerWithId p)
      let most = foldr (max . snd) 0 counts
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "dungeonWarg.attack"
        causedBy attrs
        targets [p | (p, n) <- counts, n == most] \p -> immediateAttack p attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      pure e
    _ -> DungeonWarg <$> runMessage msg attrs
