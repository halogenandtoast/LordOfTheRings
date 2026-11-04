module LordOfTheRings.Enemy.Cards.Tom (tom) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (runTroll, trollAbilities, trollEnemy)

newtype Tom = Tom EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

tom :: EnemyCard Tom
tom = enemy Tom Cards.tom

-- | Troll enemies can only be attacked by one character at a time.
instance HasModifiersFor Tom where
  getModifiersFor (Tom a) = do
    trolls <- select trollEnemy
    for_ trolls \t -> modified a t [AttackedByOneCharacterAtATime]

-- | Forced: After Tom engages a player, sack 1.
-- Forced: Return Tom to the staging area at the end of the combat phase. The engaged
-- player may raise his threat by 1 to cancel this effect.
instance HasAbilities Tom where
  getAbilities (Tom a) = trollAbilities a

instance RunMessage Tom where
  runMessage msg (Tom attrs) = Tom <$> runTroll 1 msg attrs
