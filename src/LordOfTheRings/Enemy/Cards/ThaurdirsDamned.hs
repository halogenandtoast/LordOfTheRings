module LordOfTheRings.Enemy.Cards.ThaurdirsDamned (thaurdirsDamned) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (discardTopCards, printedCost)

newtype ThaurdirsDamned = ThaurdirsDamned EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thaurdirsDamned :: EnemyCard ThaurdirsDamned
thaurdirsDamned = enemy ThaurdirsDamned Cards.thaurdirsDamned

-- | Forced: After Thaurdir's Damned engages you, discard the top 2 cards of your deck and
-- assign X damage among characters you control. X is the combined printed cost of the
-- discarded cards.
instance HasAbilities ThaurdirsDamned where
  getAbilities (ThaurdirsDamned a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage ThaurdirsDamned where
  runMessage msg e@(ThaurdirsDamned attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> do
        x <- sum . map printedCost <$> discardTopCards pid 2
        when (x > 0) $ dealDamageDivided attrs pid x
      pure e
    _ -> ThaurdirsDamned <$> runMessage msg attrs
