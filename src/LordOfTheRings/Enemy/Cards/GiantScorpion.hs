module LordOfTheRings.Enemy.Cards.GiantScorpion (giantScorpion) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GiantScorpion = GiantScorpion EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantScorpion :: EnemyCard GiantScorpion
giantScorpion = enemy GiantScorpion Cards.giantScorpion

{- | Forced: After Giant Scorpion damages a character, that character cannot ready until
the end of the round: read as the damage is dealt, as Muck Adder's.
Shadow: If this attack deals damage, the defending character cannot ready until the end of
the round: read off the damage the attack goes on to deal.
-}
instance RunMessage GiantScorpion where
  runMessage msg e@(GiantScorpion attrs) = case msg of
    DoStep 1 (DealDamage (sourceEnemy -> Just eid) target n) | eid == attrs.id, n > 0, isCharacter target -> do
      roundModifier attrs target CannotReady
      GiantScorpion <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> attackModifier (CardIdSource cid) eid (AttackDamageModifiesUntilEndOfRound d CannotReady)
      pure e
    _ -> GiantScorpion <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
