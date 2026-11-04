module LordOfTheRings.Enemy.Cards.Snaga (snaga) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (printedCost)

newtype Snaga = Snaga EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Archery 1, as its card def says.
snaga :: EnemyCard Snaga
snaga = enemy Snaga Cards.snaga

-- | When Revealed: Discard 1 random card from your hand. Snaga gets +X threat until the end
-- of the round, where X is the discarded card's cost.
instance RunMessage Snaga where
  runMessage msg e@(Snaga attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      hand <- (.hand) <$> getPlayer you
      discarded <- headMay <$> shuffleM hand
      for_ discarded \card -> do
        discardFromHand you card
        let x = printedCost card
        when (x > 0) $ roundModifier attrs attrs (ThreatModifier x)
      pure e
    _ -> Snaga <$> runMessage msg attrs
