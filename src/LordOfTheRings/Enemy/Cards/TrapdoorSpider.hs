module LordOfTheRings.Enemy.Cards.TrapdoorSpider (trapdoorSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (lostTriggered)

newtype TrapdoorSpider = TrapdoorSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trapdoorSpider :: EnemyCard TrapdoorSpider
trapdoorSpider = enemy TrapdoorSpider Cards.trapdoorSpider

-- | Forced: If Trapdoor Spider damages a character, discard that character from play.
-- Lost: Trapdoor Spider engages the last player and makes an immediate attack.
instance RunMessage TrapdoorSpider where
  runMessage msg e@(TrapdoorSpider attrs) = case msg of
    -- the damage as it is dealt, past any cancel; the character goes once it is placed
    DoStep 1 (DealDamage (EnemyAttackSource eid) target n) | eid == attrs.id && n > 0 -> do
      discard target
      pure e
    _ | lostTriggered attrs msg -> do
      lastPlayer <- getLastPlayer
      engageEnemy lastPlayer attrs.id
      immediateAttack lastPlayer attrs.id
      pure e
    _ -> TrapdoorSpider <$> runMessage msg attrs
