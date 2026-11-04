module LordOfTheRings.Enemy.Cards.TheWitchKing (theWitchKing) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)

newtype TheWitchKing = TheWitchKing EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWitchKing :: EnemyCard TheWitchKing
theWitchKing = enemy TheWitchKing Cards.theWitchKing

-- | Immune to player card effects. The Witch-king gets -30 engagement cost while The One
-- Ring is exhausted. Characters with 1 willpower or less cannot defend attacks made by The
-- Witch-king.
instance HasModifiersFor TheWitchKing where
  getModifiersFor (TheWitchKing a) = do
    exhausted <- theOneRingExhausted
    modifySelf a $ ImmuneToPlayerCardEffects : [EngagementCostModifier (-30) | exhausted]
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> do
      willpower <- getWillpower c
      when (willpower <= 1) $ modified a c [CannotDefendAgainst (EnemyWithId a.id)]

instance RunMessage TheWitchKing where
  runMessage msg (TheWitchKing attrs) = TheWitchKing <$> runMessage msg attrs
