module LordOfTheRings.Enemy.Cards.GiantSpider (giantSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GiantSpider = GiantSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantSpider :: EnemyCard GiantSpider
giantSpider = enemy GiantSpider Cards.giantSpider

{- | Forced: After Giant Spider attacks and damages a character, that character cannot ready
until the end of the round: read as its attack's damage is dealt, as Giant Scorpion's.
Shadow: Defending character cannot ready until the end of the round.
-}
instance RunMessage GiantSpider where
  runMessage msg e@(GiantSpider attrs) = case msg of
    DoStep 1 (DealDamage (EnemyAttackSource eid) target n) | eid == attrs.id, n > 0, isCharacter target -> do
      roundModifier attrs target CannotReady
      GiantSpider <$> runMessage msg attrs
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier (CardIdSource cid) d CannotReady
      pure e
    _ -> GiantSpider <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
