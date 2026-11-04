module LordOfTheRings.Enemy.Cards.Bert (bert) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (runTroll, trollAbilities, trollEnemy)

newtype Bert = Bert EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

bert :: EnemyCard Bert
bert = enemy Bert Cards.bert

-- | Players cannot play attachment cards on Troll enemies.
instance HasModifiersFor Bert where
  getModifiersFor (Bert a) = do
    trolls <- select trollEnemy
    for_ trolls \t -> modified a t [CannotHavePlayerAttachments]

-- | Forced: After Bert engages a player, sack 1.
-- Forced: Return Bert to the staging area at the end of the combat phase. The engaged
-- player may raise his threat by 1 to cancel this effect.
instance HasAbilities Bert where
  getAbilities (Bert a) = trollAbilities a

instance RunMessage Bert where
  runMessage msg (Bert attrs) = Bert <$> runTroll 1 msg attrs
