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
      players <- getPlayerOrder
      pushAll [ChooseAndExhaust (toSource attrs) pid 1 | pid <- players]
      pure e
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      push $ ChooseAndExhaust (EnemySource eid) pid (if isJust defender then 1 else 2)
      pure e
    _ -> KingSpider <$> runMessage msg attrs
