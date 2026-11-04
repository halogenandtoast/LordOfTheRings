module LordOfTheRings.Enemy.Cards.William (william) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (runTroll, trollAbilities, trollEnemy)

newtype William = William EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

william :: EnemyCard William
william = enemy William Cards.william

-- | Troll enemies not engaged with a player cannot take damage.
instance HasModifiersFor William where
  getModifiersFor (William a) = do
    trolls <- select (trollEnemy <> NotEnemy (EnemyEngagedWith AnyPlayer))
    for_ trolls \t -> modified a t [CannotTakeDamage]

-- | Forced: After William engages a player, sack 2.
-- Forced: Return William to the staging area at the end of the combat phase. The engaged
-- player may raise his threat by 1 to cancel this effect.
instance HasAbilities William where
  getAbilities (William a) = trollAbilities a

instance RunMessage William where
  runMessage msg (William attrs) = William <$> runTroll 2 msg attrs
