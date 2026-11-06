module LordOfTheRings.Enemy.Cards.LongTrailer (longTrailer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype LongTrailer = LongTrailer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

longTrailer :: EnemyCard LongTrailer
longTrailer = enemy LongTrailer Cards.longTrailer

instance HasAbilities LongTrailer where
  getAbilities (LongTrailer a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow After | a.placement == StagingArea]

instance RunMessage LongTrailer where
  runMessage msg e@(LongTrailer a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      immediateAttack lead a.id
      pure e
    _ -> LongTrailer <$> runMessage msg a
