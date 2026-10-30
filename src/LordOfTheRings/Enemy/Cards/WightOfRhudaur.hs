module LordOfTheRings.Enemy.Cards.WightOfRhudaur (wightOfRhudaur) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (currentQuestProgress)

newtype WightOfRhudaur = WightOfRhudaur EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wightOfRhudaur :: EnemyCard WightOfRhudaur
wightOfRhudaur = enemy WightOfRhudaur Cards.wightOfRhudaur

-- | While the current quest has 1 or more progress on it, Wight of Rhudaur gains surge.
instance HasModifiersFor WightOfRhudaur where
  getModifiersFor (WightOfRhudaur a) = do
    progress <- currentQuestProgress
    when (progress > 0) $ modified a a [GainKeyword Surge]

-- | While the current quest has 0 progress on it, Wight of Rhudaur gains doomed 2. Doomed
-- resolves as it is revealed, so its card raises the threat then.
instance RunMessage WightOfRhudaur where
  runMessage msg e@(WightOfRhudaur attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      progress <- currentQuestProgress
      when (progress == 0) $ raiseThreatFromDoomed 2
      pure e
    _ -> WightOfRhudaur <$> runMessage msg attrs
