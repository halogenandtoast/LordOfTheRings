module LordOfTheRings.Enemy.Cards.BlackNumenorean (blackNumenorean) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, hiddenCardCount)

newtype BlackNumenorean = BlackNumenorean EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackNumenorean :: EnemyCard BlackNumenorean
blackNumenorean = enemy BlackNumenorean Cards.blackNumenorean

-- | X is the total number of hidden cards in the game.
instance HasModifiersFor BlackNumenorean where
  getModifiersFor (BlackNumenorean a) = do
    x <- hiddenCardCount
    when (x > 0) $ modified a a [ThreatModifier x]

-- | Forced: At the end of the round, each player must raise his threat by 1 for each hidden
-- card in his play area.
instance HasAbilities BlackNumenorean where
  getAbilities (BlackNumenorean a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage BlackNumenorean where
  runMessage msg e@(BlackNumenorean attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid . length =<< getHiddenCards pid
      pure e
    _ -> BlackNumenorean <$> runMessage msg attrs
