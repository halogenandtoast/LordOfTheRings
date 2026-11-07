module LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers
  (cityStrength, damageAnnuminas, repairAnnuminas, eachAnnuminasTeam, otherAnnuminasTeams, setAsideAnnuminasCards, annuminasStages, annuminasVictoryCopies, chooseAnnuminasStage) where

import Data.List (nubBy)
import LordOfTheRings.Card (CardCode, HasCardCode, toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.I18n (textVar)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Message (Message (ContributeShared, ResolveEpicEffect, QueueEpicCommand))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target (Target (ScenarioTarget), Targetable)

annuminasStages :: HasGame m => m [(GroupOrdinal, Text, EpicStage)]
annuminasStages = getsGame $ \g ->
  let replica = scenarioEpic $ toAttrs g.scenario
      own = maybe (GroupOrdinal 0) epicGroup replica
      boards = maybe [] epicBoards replica
      name = fromMaybe "" $ epicBoardName <$> find ((== own) . epicBoardGroup) boards
      current = epicBoardFor own name g
   in [(b.epicBoardGroup, b.epicBoardName, stage) | b <- current : filter ((/= own) . epicBoardGroup) boards, Just stage <- [b.epicBoardStage]]

-- Only explicitly global scenario effects reach another team. Each destination
-- receives its own durable command; its engine resolves against its own deck.
eachAnnuminasTeam :: ReverseQueue m => CardCode -> EpicEffect -> m ()
eachAnnuminasTeam code effect = do
  push $ ResolveEpicEffect code effect ScenarioTarget
  otherAnnuminasTeams code effect

otherAnnuminasTeams :: ReverseQueue m => CardCode -> EpicEffect -> m ()
otherAnnuminasTeams code effect = do
  replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
  for_ replica $ \r ->
    for_ (nub [b.epicBoardGroup | b <- r.epicBoards, b.epicBoardGroup /= r.epicGroup]) $ \destination ->
      push $ QueueEpicCommand destination code effect ScenarioTarget

cityStrength :: HasGame m => m Int
cityStrength = getsGame $ \g ->
  fromMaybe
    (sum [o.resources | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.annuminas])
    (scenarioEpic (toAttrs g.scenario) >>= annuminasCityStrength . epicShared)

damageAnnuminas, repairAnnuminas :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
damageAnnuminas source amount = changeCityStrength source (negate amount)
repairAnnuminas = changeCityStrength

changeCityStrength :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
changeCityStrength source amount = do
  shared <- getsGame $ scenarioEpic . toAttrs . (.scenario)
  if isJust (shared >>= annuminasCityStrength . epicShared)
    then push $ ContributeShared AnnuminasCityStrength amount
    else do
      cities <- getsGame $ \g -> [toAttrs o | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.annuminas]
      for_ cities $ \oid ->
        if amount < 0 then dealDamage source oid (negate amount) else placeResources source oid amount

setAsideAnnuminasCards :: ReverseQueue m => m ()
setAsideAnnuminasCards = do
  deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
  let waiting = map toCardCode
        [ Enemies.lieutenantOfAngmar, Locations.batteringRam, Locations.enemyCamp
        , Locations.siegeTunnel, Enemies.hostOfAngmar, Objectives.dunedainOfAnnuminas
        ]
  traverse_ setAside $ filter ((`elem` waiting) . toCardCode) deck

-- Count physical copies across victory displays, retaining duplicate card codes.
annuminasVictoryCopies :: HasGame m => CardCode -> m Int
annuminasVictoryCopies code = getsGame $ \g ->
  let replica = scenarioEpic $ toAttrs g.scenario
      own = maybe (GroupOrdinal 0) epicGroup replica
      boards = nubBy (\a b -> a.epicBoardGroup == b.epicBoardGroup)
        [b | r <- maybeToList replica, b <- r.epicBoards, b.epicBoardGroup /= own]
      localCards = map toCardCode (toAttrs g.scenario).victoryDisplay
      remoteCards = [c | b <- boards, stage <- maybeToList b.epicBoardStage, c <- stage.epicStageVictoryCards]
   in length $ filter (== code) $ localCards <> remoteCards

chooseAnnuminasStage
  :: (ReverseQueue m, Targetable source, HasCardCode source)
  => Message -> PlayerId -> source -> EpicEffect -> m ()
chooseAnnuminasStage rebuild pid source effect = do
  stages <- annuminasStages
  own <- getsGame $ maybe (GroupOrdinal 0) epicGroup . scenarioEpic . toAttrs . (.scenario)
  chooseOneRebuildingM pid rebuild do
    questionLabeled "annuminas.chooseStage"
    causedBy source
    for_ stages $ \(ordinal, name, stage) -> when (notNull stage.epicStageQuests
      && (effect /= EpicHealControlledCharacter || stage.epicStageHealingAvailable)
      && (effect /= EpicDrawCards || stage.epicStageDrawingAvailable)) $
      if ordinal == own
        then labeled "epic.thisGroup" $ push $ ResolveEpicEffect (toCardCode source) effect ScenarioTarget
        else labeledWith "epic.group" [textVar "group" name] $ push $ QueueEpicCommand ordinal (toCardCode source) effect ScenarioTarget
