module LordOfTheRings.Enemy.Cards.GiantMarshWorm (giantMarshWorm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GiantMarshWorm = GiantMarshWorm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantMarshWorm :: EnemyCard GiantMarshWorm
giantMarshWorm = enemy GiantMarshWorm Cards.giantMarshWorm

-- | Forced: Remove 2 damage from Giant Marsh Worm at the end of each round.
instance HasAbilities GiantMarshWorm where
  getAbilities (GiantMarshWorm a) = [mkAbility a 1 $ forced RoundEndsWindow | a.damage > 0]

instance RunMessage GiantMarshWorm where
  runMessage msg e@(GiantMarshWorm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTokens attrs attrs Damage (min 2 attrs.damage)
      pure e
    _ -> GiantMarshWorm <$> runMessage msg attrs
