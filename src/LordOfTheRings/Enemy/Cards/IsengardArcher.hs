module LordOfTheRings.Enemy.Cards.IsengardArcher (isengardArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (playersAtThisStage)

newtype IsengardArcher = IsengardArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

isengardArcher :: EnemyCard IsengardArcher
isengardArcher = enemy IsengardArcher Cards.isengardArcher

instance HasAbilities IsengardArcher where
  getAbilities (IsengardArcher a) = [mkAbility a 1 $ forced $ CardEntersStagingAreaWindow After (toTarget a)]

instance RunMessage IsengardArcher where
  runMessage msg e@(IsengardArcher a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- playersAtThisStage
      for_ players (\pid -> dealDamageToOneCharacter a pid 2)
      pure e
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      allies <- select (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "isengardArcher.shadow"
        causedBy a
        targets allies (\ally -> dealDamage a ally 2)
      pure e
    _ -> IsengardArcher <$> runMessage msg a
