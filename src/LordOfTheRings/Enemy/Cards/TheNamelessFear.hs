module LordOfTheRings.Enemy.Cards.TheNamelessFear (theNamelessFear) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype TheNamelessFear = TheNamelessFear EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theNamelessFear :: EnemyCard TheNamelessFear
theNamelessFear = enemy TheNamelessFear Cards.theNamelessFear

-- | Immune to player card effects. The Nameless Fear cannot engage or be engaged. X is the
-- number of victory points in the victory display.
instance HasModifiersFor TheNamelessFear where
  getModifiersFor (TheNamelessFear a) = do
    x <- getsGame ((.victoryPoints) . toAttrs . (.scenario))
    modifySelf a
      [ ImmuneToPlayerCardEffects
      , NoEngagementChecks
      , CannotBeOptionallyEngagedBy AnyPlayer
      , ThreatModifier x
      , AttackModifier x
      , DefenseModifier x
      ]
