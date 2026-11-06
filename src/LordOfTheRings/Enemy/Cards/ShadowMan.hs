module LordOfTheRings.Enemy.Cards.ShadowMan (shadowMan) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype ShadowMan = ShadowMan EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowMan :: EnemyCard ShadowMan
shadowMan = enemy ShadowMan Cards.shadowMan

instance RunMessage ShadowMan where
  runMessage msg e@(ShadowMan a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      roundModifier a GameTarget (CannotPlay $ CardWithType EventType)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      time <- currentNight
      let bonus = case time of Just Midnight -> 3; Just Dusk -> 2; _ -> 1
      enemyAttackModifier a eid (AttackModifier bonus)
      pure e
    _ -> ShadowMan <$> runMessage msg a
