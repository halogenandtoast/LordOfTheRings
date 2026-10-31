module LordOfTheRings.Enemy.Cards.GoblinLurker (goblinLurker) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Dark))

newtype GoblinLurker = GoblinLurker EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinLurker :: EnemyCard GoblinLurker
goblinLurker = enemy GoblinLurker Cards.goblinLurker

darkActive :: HasGame m => m Bool
darkActive = selectAny (IsActiveLocation <> LocationWithTrait Dark)

-- | Cannot be optionally engaged. If the active location is a Dark location, Goblin Lurker
-- gets +3 threat and -40 engagement cost.
instance HasModifiersFor GoblinLurker where
  getModifiersFor (GoblinLurker a) = do
    dark <- darkActive
    modifySelf a
      $ CannotBeOptionallyEngagedBy AnyPlayer
      : (if dark then [ThreatModifier 3, EngagementCostModifier (-40)] else [])

-- | Shadow: Attacking enemy gets +1 attack (+3 attack instead if the active location is a
-- Dark location).
instance RunMessage GoblinLurker where
  runMessage msg e@(GoblinLurker attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      dark <- darkActive
      enemyGetsAttack attrs eid (if dark then 3 else 1)
      pure e
    _ -> GoblinLurker <$> runMessage msg attrs
