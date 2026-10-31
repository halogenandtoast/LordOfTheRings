module LordOfTheRings.Enemy.Cards.BlackNumenoreanNightmare (blackNumenoreanNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, hiddenCardCount, turnHiddenCardsFaceup)

newtype BlackNumenoreanNightmare = BlackNumenoreanNightmare EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

blackNumenoreanNightmare :: EnemyCard BlackNumenoreanNightmare
blackNumenoreanNightmare = enemy BlackNumenoreanNightmare Cards.blackNumenoreanNightmare

-- | X is the total number of hidden cards in the game.
instance HasModifiersFor BlackNumenoreanNightmare where
  getModifiersFor (BlackNumenoreanNightmare a) = do
    x <- hiddenCardCount
    when (x > 0) $ modified a a [ThreatModifier x]

-- | Forced: When Black Númenórean engages you, turn all hidden cards in your play area
-- faceup.
-- Forced: At the end of the round, each player must raise his threat by 1 for each hidden
-- card in his play area.
instance HasAbilities BlackNumenoreanNightmare where
  getAbilities (BlackNumenoreanNightmare a) =
    [ mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id
    , mkAbility a 2 $ forced RoundEndsWindow
    ]

instance RunMessage BlackNumenoreanNightmare where
  runMessage msg e@(BlackNumenoreanNightmare attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] (void . turnHiddenCardsFaceup)
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid . length =<< getHiddenCards pid
      pure e
    _ -> BlackNumenoreanNightmare <$> runMessage msg attrs
