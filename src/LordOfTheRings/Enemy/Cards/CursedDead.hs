module LordOfTheRings.Enemy.Cards.CursedDead (cursedDead) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (copyInOwnersDiscard, encounterDiscardPile)

newtype CursedDead = CursedDead EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedDead :: EnemyCard CursedDead
cursedDead = enemy CursedDead Cards.cursedDead

-- | When Revealed: Put each copy of Cursed Dead in the discard pile into play in the
-- staging area.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if there is a copy of the
-- defending character in its owner's discard pile).
instance RunMessage CursedDead where
  runMessage msg e@(CursedDead attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      copies <- filter (cardMatches $ cardIs Cards.cursedDead) <$> encounterDiscardPile
      traverse_ addToStagingFromDiscard copies
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      copied <- anyM copyInOwnersDiscard defenders
      enemyGetsAttack attrs eid if copied then 2 else 1
      pure e
    _ -> CursedDead <$> runMessage msg attrs
