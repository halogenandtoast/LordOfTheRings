module LordOfTheRings.Enemy.Cards.CursedDeadNightmare (cursedDeadNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (copyInOwnersDiscard, encounterDiscardPile)

newtype CursedDeadNightmare = CursedDeadNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedDeadNightmare :: EnemyCard CursedDeadNightmare
cursedDeadNightmare = enemy CursedDeadNightmare Cards.cursedDeadNightmare

-- | When Revealed: Put each copy of Cursed Dead in the discard pile into play in the
-- staging area: each card of its title, the standard one's copies too.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if there is a copy of the
-- defending character in its owner's discard pile).
instance RunMessage CursedDeadNightmare where
  runMessage msg e@(CursedDeadNightmare attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      copies <- filter (cardMatches $ CardWithTitle "Cursed Dead") <$> encounterDiscardPile
      traverse_ addToStagingFromDiscard copies
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      copied <- anyM copyInOwnersDiscard defenders
      enemyGetsAttack attrs eid if copied then 2 else 1
      pure e
    _ -> CursedDeadNightmare <$> runMessage msg attrs
