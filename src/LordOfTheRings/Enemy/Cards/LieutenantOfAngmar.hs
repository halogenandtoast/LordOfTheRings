module LordOfTheRings.Enemy.Cards.LieutenantOfAngmar (lieutenantOfAngmar) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Epic.Types
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (annuminasStages)
import LordOfTheRings.Scenario.Types (scenarioEpic)

newtype LieutenantOfAngmar = LieutenantOfAngmar EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

lieutenantOfAngmar :: EnemyCard LieutenantOfAngmar
lieutenantOfAngmar = enemy LieutenantOfAngmar Cards.lieutenantOfAngmar

instance HasModifiersFor LieutenantOfAngmar where
  getModifiersFor (LieutenantOfAngmar a) = do
    progress <- getMainQuestProgress
    modifySelf a $ [ImmuneToPlayerCardEffects] <> [CannotTakeDamage | progress < 10]

instance HasAbilities LieutenantOfAngmar where
  getAbilities (LieutenantOfAngmar a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage LieutenantOfAngmar where
  runMessage msg e@(LieutenantOfAngmar a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      stages <- annuminasStages
      let lowest = minimumMay [stage.epicStageThreat | (_, _, stage) <- stages]
          candidates = [(ordinal, name) | (ordinal, name, stage) <- stages, Just stage.epicStageThreat == lowest]
          move ordinal = MoveAnnuminasEnemy a.id ordinal EpicAttackFirstPlayer
      case candidates of
        [(ordinal, _)] -> push $ move ordinal
        _ -> do
          lead <- getLead
          own <- getsGame $ maybe (GroupOrdinal 0) epicGroup . scenarioEpic . toAttrs . (.scenario)
          chooseOneRebuildingM lead msg do
            questionLabeled "annuminas.lieutenantStage"
            causedBy a
            for_ candidates $ \(ordinal, name) ->
              if ordinal == own
                then labeled "epic.thisGroup" $ push $ move ordinal
                else labeledWith "epic.group" [textVar "group" name] $ push $ move ordinal
      pure e
    _ -> LieutenantOfAngmar <$> runMessage msg a
