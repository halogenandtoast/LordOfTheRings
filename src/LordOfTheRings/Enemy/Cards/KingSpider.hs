module LordOfTheRings.Enemy.Cards.KingSpider (kingSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype KingSpider = KingSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

kingSpider :: EnemyCard KingSpider
kingSpider = enemy KingSpider Cards.kingSpider

instance RunMessage KingSpider where
  runMessage msg e@(KingSpider attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> chooseAndExhaust attrs pid 1
      pure e
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      chooseAndExhaust (EnemySource eid) pid (ifUndefended defender 2 1)
      pure e
    _ -> KingSpider <$> runMessage msg attrs
