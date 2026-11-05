module LordOfTheRings.Enemy.Cards.ZirakzigilArcher (zirakzigilArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (printedCost)

newtype ZirakzigilArcher = ZirakzigilArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

zirakzigilArcher :: EnemyCard ZirakzigilArcher
zirakzigilArcher = enemy ZirakzigilArcher Cards.zirakzigilArcher

instance RunMessage ZirakzigilArcher where
  runMessage msg e@(ZirakzigilArcher a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      discardRandomFromHandWithResult a you 1
      pure e
    DiscardedRandomFromHand (isSource a -> True) pid cards -> do
      characters <- charactersControlledBy pid
      dealDamageDividedAmong a pid characters (sum $ map printedCost cards)
      pure e
    _ -> ZirakzigilArcher <$> runMessage msg a
