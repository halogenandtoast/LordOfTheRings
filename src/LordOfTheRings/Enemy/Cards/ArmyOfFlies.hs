module LordOfTheRings.Enemy.Cards.ArmyOfFlies (armyOfFlies) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ArmyOfFlies = ArmyOfFlies EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

armyOfFlies :: EnemyCard ArmyOfFlies
armyOfFlies = enemy ArmyOfFlies Cards.armyOfFlies

instance HasAbilities ArmyOfFlies where
  getAbilities (ArmyOfFlies a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage ArmyOfFlies where
  runMessage msg e@(ArmyOfFlies a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] $ discardNonObjectiveAttachment a
      pure e
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardNonObjectiveAttachment a pid
      pure e
    _ -> ArmyOfFlies <$> runMessage msg a
