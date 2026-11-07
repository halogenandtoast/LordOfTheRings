module LordOfTheRings.Epic.Board (epicBoardFor, matchesEpicTarget, matchesEpicGroup, epicVictoryCopies) where

import Data.Map.Strict qualified as Map
import Data.List (nubBy)
import LordOfTheRings.Card
import LordOfTheRings.Ally.Types (AllyAttrs (allyExhausted))
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame, runReadGame)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Drawing (canDrawCardsFrom)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (getRemainingPlayers)
import LordOfTheRings.Helpers.Healing (healableCharacters)
import LordOfTheRings.Source (Source (CardCodeSource))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Matcher.Types (HeroMatcher (HeroControlledBy), AllyMatcher (AllyControlledBy), PlayerMatcher (AnyPlayer))
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Matcher (getMainQuestCards)
import LordOfTheRings.Matcher.Types (CardMatcher (CardWithTrait), cardMatches)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Modifier (Modifier (modifierType), ModifierType (ConsideredOutOfPlay))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioSetAside, scenarioVictoryDisplay)
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Basic, Ship))

-- The snapshot includes only faceup public encounter entities. No player's hand,
-- deck, hidden cards or encounter-deck order crosses a group boundary.
epicBoardFor :: GroupOrdinal -> Text -> Game -> EpicBoard
epicBoardFor ordinal name g
  | isJust g.outcome = EpicBoard ordinal name [] False Nothing
  | otherwise = EpicBoard ordinal name targets hasWarship (Just stage)
 where
  stage = EpicStage
    (runReadGame getStagingThreat g)
    [ EpicQuest (toCardCode q) q.stage q.progress
        (cdQuestPoints (toCardDef q) $> runReadGame (getQuestPoints $ toAttrs q) g)
        q.flipped
    | q <- runReadGame getMainQuestCards g
    ]
    (map toCardCode $ scenarioVictoryDisplay $ toAttrs g.scenario)
    (runReadGame (notNull <$> healableCharacters (CardCodeSource $ toCardCode Quests.defendTheCityAnnuminas) (HeroControlledBy AnyPlayer) (AllyControlledBy AnyPlayer)) g)
    (length $ filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) $ scenarioSetAside $ toAttrs g.scenario)
    (runReadGame (anyM (canDrawCardsFrom $ CardCodeSource $ toCardCode Quests.manTheWallsAnnuminas) =<< getRemainingPlayers) g)
    [EpicPlayer pid player.name | pid <- g.playerOrder, Just player <- [Map.lookup pid g.players], not player.eliminated]
  asTarget target kind engaged damage exhausted upgradeable card =
    EpicTarget
      target
      (MkEncounterCard (toCard card).id $ toCardCode card)
      kind
      (cdUnique $ toCardDef card)
      damage
      exhausted
      upgradeable
      engaged
  targets =
    [ asTarget
        (toTarget e)
        EpicEnemy
        (case e.placement of EngagedWith _ -> True; _ -> False)
        0
        False
        False
        e
    | e <- toList g.entities.enemies
    , present (toTarget e)
    ]
      <> [asTarget (toTarget l) EpicLocation False 0 False False l | l <- toList g.entities.locations, present (toTarget l)]
      <> [ asTarget
             (toTarget a)
             EpicShip
             False
             a.damage
             (toAttrs a).allyExhausted
             (Basic `member` cdCardTraits (toCardDef a))
             a
         | a <- toList g.entities.allies
         , present (toTarget a)
         , cdCardType (toCardDef a) == ObjectiveAllyType
         , cardMatches (CardWithTrait Ship) a
         ]
  present target = ConsideredOutOfPlay `notElem` map modifierType (Map.findWithDefault [] target g.gameModifiers)
  hasWarship =
    any
      ((== toCardCode Objectives.gondorianWarship) . toCardCode)
      (scenarioSetAside $ toAttrs g.scenario)

matchesEpicTarget :: EpicTargetQuery -> EpicTarget -> Bool
matchesEpicTarget EpicNonUniqueEnemy t = t.epicTargetKind == EpicEnemy && not t.epicTargetUnique
matchesEpicTarget EpicOtherNonUniqueNoVictoryEnemy t =
  t.epicTargetKind == EpicEnemy && not t.epicTargetUnique && fromMaybe 0 (cdVictoryPoints $ toCardDef t.epicTargetCard) == 0
matchesEpicTarget EpicEngagedNonUniqueEnemy t = t.epicTargetKind == EpicEnemy && t.epicTargetEngaged && not t.epicTargetUnique
matchesEpicTarget EpicShip t =
  t.epicTargetKind
    == EpicShip
    && (t.epicTargetDamage > 0 || t.epicTargetExhausted || t.epicTargetUpgradeable)
matchesEpicTarget query t = query == t.epicTargetKind

matchesEpicGroup :: EpicTargetQuery -> EpicBoard -> Bool
matchesEpicGroup EpicWarshipGroup board = board.epicBoardHasWarship
matchesEpicGroup query board = maybe False eligible board.epicBoardStage
 where
  eligible stage = notNull stage.epicStageQuests && case query of
    EpicHealingGroup -> stage.epicStageHealingAvailable
    EpicDrawingGroup -> stage.epicStageDrawingAvailable
    _ -> False

-- A local victory display is read directly, rather than through its old public snapshot.
epicVictoryCopies :: HasGame m => CardCode -> m Int
epicVictoryCopies code = getsGame $ \g ->
  let replica = scenarioEpic $ toAttrs g.scenario
      own = maybe (GroupOrdinal 0) epicGroup replica
      boards = nubBy (\a b -> a.epicBoardGroup == b.epicBoardGroup)
        [b | r <- maybeToList replica, b <- r.epicBoards, b.epicBoardGroup /= own]
      localCards = map toCardCode (toAttrs g.scenario).victoryDisplay
      remoteCards = [c | b <- boards, stage <- maybeToList b.epicBoardStage, c <- stage.epicStageVictoryCards]
   in length $ filter (== code) $ localCards <> remoteCards
