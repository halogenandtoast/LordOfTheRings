module LordOfTheRings.Epic.Board (epicBoardFor, matchesEpicTarget) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Ally.Types (AllyAttrs (allyExhausted))
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (runReadGame)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Matcher (getMainQuestCards)
import LordOfTheRings.Matcher.Types (CardMatcher (CardWithTrait), cardMatches)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Modifier (Modifier (modifierType), ModifierType (ConsideredOutOfPlay))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario.Types (scenarioSetAside, scenarioVictoryDisplay)
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
matchesEpicTarget EpicEngagedNonUniqueEnemy t = t.epicTargetKind == EpicEnemy && t.epicTargetEngaged && not t.epicTargetUnique
matchesEpicTarget EpicShip t =
  t.epicTargetKind
    == EpicShip
    && (t.epicTargetDamage > 0 || t.epicTargetExhausted || t.epicTargetUpgradeable)
matchesEpicTarget query t = query == t.epicTargetKind
