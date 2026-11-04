module LordOfTheRings.Enemy.Cards.GiantCentipede (giantCentipede) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GiantCentipede = GiantCentipede EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantCentipede :: EnemyCard GiantCentipede
giantCentipede = enemy GiantCentipede Cards.giantCentipede

-- | Forced: After Giant Centipede damages a character, treat that character's text box as
-- if it were blank until the end of the round (except for Traits): read as the damage is
-- dealt, as Giant Scorpion's.
-- Shadow: Treat the defending character's text box as blank until the end of the round.
instance RunMessage GiantCentipede where
  runMessage msg e@(GiantCentipede attrs) = case msg of
    DoStep 1 (DealDamage (sourceEnemy -> Just eid) target n) | eid == attrs.id, n > 0, isCharacter target -> do
      roundModifier attrs target Blank
      GiantCentipede <$> runMessage msg attrs
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier attrs d Blank
      pure e
    _ -> GiantCentipede <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
