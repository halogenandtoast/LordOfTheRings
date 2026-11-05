module LordOfTheRings.Enemy.Cards.OrthancArcher (orthancArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrthancArcher = OrthancArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orthancArcher :: EnemyCard OrthancArcher
orthancArcher = enemy OrthancArcher Cards.orthancArcher

instance RunMessage OrthancArcher where
  runMessage msg e@(OrthancArcher a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      discardRandomFromHandWithResult a you 1
      pure e
    DiscardedRandomFromHand (isSource a -> True) pid cards -> do
      characters <- charactersControlledBy pid
      dealDamageDividedAmong a pid characters (sum $ map printedCostOf cards)
      pure e
    _ -> OrthancArcher <$> runMessage msg a
