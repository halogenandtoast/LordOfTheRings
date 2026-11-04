module LordOfTheRings.Enemy.Cards.GoblinBentSwords (goblinBentSwords) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim.Helpers (greatGoblinDefeated)

newtype GoblinBentSwords = GoblinBentSwords EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinBentSwords :: EnemyCard GoblinBentSwords
goblinBentSwords = enemy GoblinBentSwords Cards.goblinBentSwords

-- | If The Great Goblin is in the victory display, this card gains surge.
instance HasModifiersFor GoblinBentSwords where
  getModifiersFor (GoblinBentSwords a) = whenM greatGoblinDefeated $ modifySelf a [GainKeyword Surge]

-- | Shadow: attacking enemy gets +1 attack. (+2 attack instead if The Great Goblin is in the
-- victory display.)
instance RunMessage GoblinBentSwords where
  runMessage msg e@(GoblinBentSwords attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      defeated <- greatGoblinDefeated
      enemyGetsAttack attrs eid (if defeated then 2 else 1)
      pure e
    _ -> GoblinBentSwords <$> runMessage msg attrs
