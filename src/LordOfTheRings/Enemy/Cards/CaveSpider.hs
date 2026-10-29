module LordOfTheRings.Enemy.Cards.CaveSpider (caveSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Player.Types (discardableHand)

newtype CaveSpider = CaveSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caveSpider :: EnemyCard CaveSpider
caveSpider = enemy CaveSpider Cards.caveSpider

-- | Forced: After Cave Spider engages a player, that player must choose and discard 1 card
-- from his hand, if able.
instance HasAbilities CaveSpider where
  getAbilities (CaveSpider a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | When Revealed: The first player draws 1 card. Then, that player must choose and discard
-- 4 cards from his hand, if able.
instance RunMessage CaveSpider where
  runMessage msg e@(CaveSpider attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      drawCards lead 1
      doStep 1 msg
      pure e
    -- "if able": with fewer than 4 cards in hand the discard is ignored (ruling 3.101)
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      lead <- getLead
      hand <- discardableHand <$> getPlayer lead
      when (length hand >= 4) $ chooseAndDiscardFromHand lead attrs 4
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> chooseAndDiscardFromHand pid attrs 1
      pure e
    _ -> CaveSpider <$> runMessage msg attrs
