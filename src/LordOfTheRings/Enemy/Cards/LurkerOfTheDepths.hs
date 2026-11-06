module LordOfTheRings.Enemy.Cards.LurkerOfTheDepths (lurkerOfTheDepths) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype LurkerOfTheDepths = LurkerOfTheDepths EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lurkerOfTheDepths :: EnemyCard LurkerOfTheDepths
lurkerOfTheDepths = enemy LurkerOfTheDepths Cards.lurkerOfTheDepths

instance HasModifiersFor LurkerOfTheDepths where
  getModifiersFor (LurkerOfTheDepths a) = do
    players <- getPlayerOrder
    modifySelf a $
      [CannotHaveAttachments, NoEngagementChecks, CannotBeOptionallyEngagedBy AnyPlayer, DamageLimitEachRound 4]
        <> map CannotEngagePlayer players
        <> [EngagedWithEachPlayer AnyPlayer | a.placement == StagingArea]

instance RunMessage LurkerOfTheDepths where
  runMessage msg (LurkerOfTheDepths a) = LurkerOfTheDepths <$> runMessage msg a
