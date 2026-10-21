module LordOfTheRings.Enemy.Cards.MuckAdder (muckAdder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MuckAdder = MuckAdder EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

muckAdder :: EnemyCard MuckAdder
muckAdder = enemy MuckAdder Cards.muckAdder

-- | Forced: If Muck Adder damages a character, discard that character from play.
-- Shadow: Defending character gets -1 defense for the duration of this attack.
instance RunMessage MuckAdder where
  runMessage msg e@(MuckAdder attrs) = case msg of
    DealDamage (EnemySource eid) target n | eid == attrs.id, n > 0, isCharacter target -> do
      discard target
      pure e
    ResolveShadowEffect cid _ _ (Just defender) | cid == attrs.cardId -> do
      phaseModifier attrs defender (DefenseModifier (-1))
      pure e
    _ -> MuckAdder <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
