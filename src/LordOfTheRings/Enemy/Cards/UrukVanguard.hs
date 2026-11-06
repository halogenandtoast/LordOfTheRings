module LordOfTheRings.Enemy.Cards.UrukVanguard (urukVanguard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)

newtype UrukVanguard = UrukVanguard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukVanguard :: EnemyCard UrukVanguard
urukVanguard = enemy UrukVanguard Cards.urukVanguard

instance HasModifiersFor UrukVanguard where
  getModifiersFor (UrukVanguard a) = do
    crossed <- crossedAnduin
    when crossed $ modifySelf a [AttackModifier 3]

instance RunMessage UrukVanguard where
  runMessage msg e@(UrukVanguard attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      crossed <- crossedAnduin
      enemyGetsAttack attrs eid (if crossed then 2 else 1)
      pure e
    _ -> UrukVanguard <$> runMessage msg attrs
