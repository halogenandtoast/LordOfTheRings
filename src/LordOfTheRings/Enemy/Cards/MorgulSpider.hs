module LordOfTheRings.Enemy.Cards.MorgulSpider (morgulSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MorgulSpider = MorgulSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulSpider :: EnemyCard MorgulSpider
morgulSpider = enemy MorgulSpider Cards.morgulSpider

-- | When Revealed: Until the end of the round, Morgul Spider gets +1 attack for each
-- character not currently committed to a quest.
instance RunMessage MorgulSpider where
  runMessage msg e@(MorgulSpider attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      idle <- length <$> selectCharacters (not_ CommittedHero) (not_ CommittedAlly)
      when (idle > 0) $ roundModifier attrs attrs (AttackModifier idle)
      pure e
    _ -> MorgulSpider <$> runMessage msg attrs
