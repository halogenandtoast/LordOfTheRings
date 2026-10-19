{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy
import LordOfTheRings.Entities
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base as X
import LordOfTheRings.GameT
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Location
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Player.Runner as X
import LordOfTheRings.Prelude
import LordOfTheRings.Quest
import LordOfTheRings.Question
import LordOfTheRings.Scenario
import LordOfTheRings.Target
import LordOfTheRings.Treachery ()
import LordOfTheRings.Window

newGame :: Player -> [Hero] -> Game
newGame player heroes =
  Game
    { gamePlayers = Map.singleton player.id player
    , gamePlayerOrder = [player.id]
    , gameActivePlayer = player.id
    , gameFirstPlayer = player.id
    , gameEntities = mempty {entitiesHeroes = Map.fromList [(h.id, h) | h <- heroes]}
    , gameScenario = passageThroughMirkwood
    , gamePhase = ResourcePhase
    , gameRound = 0
    , gameQuestion = mempty
    , gameQuestionVersion = 0
    , gameActiveCosts = mempty
    }

answerQuestion :: PlayerId -> Int -> GameT Game
answerQuestion pid n = do
  g <- getGame
  question <- maybe (error "no question for player") pure $ Map.lookup pid g.question
  let choices = questionChoices question
  choice <- maybe (error "invalid choice") pure $ atMay choices n
  putGame $ g & questionL %~ Map.delete pid
  case question of
    ChooseOneAtATime _ | rest@(_ : _) <- take n choices <> drop (n + 1) choices -> do
      pushAll [uiToRun choice, Ask pid (ChooseOneAtATime rest)]
    _ -> push $ uiToRun choice
  mainLoop

mainLoop :: GameT Game
mainLoop = do
  mmsg <- popMessage
  g <- getGame
  case mmsg of
    Nothing -> pure g
    Just (Ask pid q) -> do
      let g' = g & questionL .~ Map.singleton pid q & questionVersionL +~ 1
      putGame g'
      pure g'
    Just msg -> do
      debug 1 $ print msg
      g' <- runMessage msg g
      putGame g'
      if notNull g'.question then pure g' else mainLoop

getPlayers :: (HasGame m) => m [Player]
getPlayers = toList <$> getsGame gamePlayers

instance RunMessage Entities where
  runMessage msg entities =
    traverseOf (heroesL . traverse) (runMessage msg) entities
      >>= traverseOf (alliesL . traverse) (runMessage msg)
      >>= traverseOf (locationsL . traverse) (runMessage msg)
      >>= traverseOf (enemiesL . traverse) (runMessage msg)
      >>= traverseOf (treacheriesL . traverse) (runMessage msg)
      >>= traverseOf (questsL . traverse) (runMessage msg)

instance RunMessage Game where
  runMessage msg g =
    traverseOf (playersL . traverse) (runMessage msg) g
      >>= traverseOf (activeCostsL . traverse) (runMessage msg)
      >>= traverseOf entitiesL (runMessage msg)
      >>= traverseOf scenarioL (runMessage msg)
      >>= runGameMessage msg

runGameMessage :: Message -> Game -> GameT Game
runGameMessage msg g = case msg of
  Run msgs -> do
    pushAll msgs
    pure g
  Setup -> do
    pushAll [DoStep n Setup | n <- [1 .. 7]]
    pure g
  BeginRound -> do
    pushAll
      $ concat
        [ [BeginPhase phase, EndPhase phase]
        | phase <- [ResourcePhase, PlanningPhase, QuestPhase, TravelPhase, EncounterPhase, CombatPhase]
        ]
    pure $ g & roundL +~ 1
  BeginPhase PlanningPhase -> do
    players <- getPlayers
    pushAll [WithTarget (toTarget player) (DoPhase PlanningPhase) | player <- players]
    pure $ g & phaseL .~ PlanningPhase
  BeginPhase phase -> pure $ g & phaseL .~ phase
  PayForCard pid card -> do
    costId <- getRandom
    push $ PayCost costId
    let activeCost =
          ActiveCost
            { activeCostId = costId
            , activeCostCost = cardCost card
            , activeCostCard = card
            , activeCostPlayer = pid
            }
    pure $ g & activeCostsL %~ insertEntity activeCost
  PaidCost costId -> pure $ g & activeCostsL %~ Map.delete costId
  PlayAlly pid card -> do
    player <- getPlayer pid
    thisAlly <- newAlly pid card
    send $ Log $ player.name <> " plays " <> display (cdName $ toCardDef card)
    push $ RunWindow $ Window After $ EnterPlay $ toTarget thisAlly
    pure $ g & entitiesL . alliesL %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  PutQuestIntoPlay card -> do
    thisQuest <- newQuest card
    pure $ g & entitiesL . questsL %~ insertEntity thisQuest
  AddToStaging card -> case cdCardType (toCardDef card) of
    EnemyType -> do
      thisEnemy <- newEnemy card
      pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) thisEnemy)
    LocationType -> do
      thisLocation <- newLocation card
      pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) thisLocation)
    _ -> error "Unhandled"
  _ -> pure g
