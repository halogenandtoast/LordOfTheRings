{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module LordOfTheRings.Game.Base) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Enemy
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Frontend
import LordOfTheRings.GameT
import LordOfTheRings.Game.Base
import LordOfTheRings.Game.Base as Cost (ActiveCost (..))
import LordOfTheRings.Game.Base as Player (Player (..))
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Target
import LordOfTheRings.Location
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Queue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Scenario
import LordOfTheRings.Window

newGame :: Player -> [Hero] -> Game
newGame player heroes =
  Game
    { players = Map.singleton player.id player
    , activePlayer = player.id
    , firstPlayer = player.id
    , entities = mempty {heroes = Map.fromList [(h.id, h) | h <- heroes]}
    , scenario = newScenario PassageThroughMirkwood
    }

getHasQuestion :: GameT Bool
getHasQuestion = any (isJust . (.question)) <$> getPlayers

mainLoop :: GameT Game
mainLoop = do
  mmsg <- popMessage
  g <- getGame
  case mmsg of
    Nothing -> pure g
    Just msg -> do
      debug 1 $ print msg
      g' <- runMessage msg g
      putGame g'

      -- do we have a question
      hasQuestion <- getHasQuestion
      if hasQuestion
        then pure g'
        else mainLoop

getPlayers :: (HasGame m) => m [Player]
getPlayers = Map.elems <$> getsGame (.players)

instance RunMessage Entities where
  runMessage msg entities = do
    heroes <- traverse (runMessage msg) entities.heroes
    allies <- traverse (runMessage msg) entities.allies
    locations <- traverse (runMessage msg) entities.locations
    enemies <- traverse (runMessage msg) entities.enemies
    pure (entities {heroes, allies, locations, enemies})

instance RunMessage Game where
  runMessage msg game = do
    players <- traverse (runMessage msg) game.players
    entities <- runMessage msg game.entities
    scenario <- runMessage msg game.scenario
    runGameMessage msg (game {players, entities, scenario})

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
      [ BeginPhase ResourcePhase
      , EndPhase ResourcePhase
      , BeginPhase PlanningPhase
      , EndPhase PlanningPhase
      , BeginPhase QuestPhase
      , EndPhase QuestPhase
      , BeginPhase TravelPhase
      , EndPhase TravelPhase
      , BeginPhase EncounterPhase
      , EndPhase EncounterPhase
      , BeginPhase CombatPhase
      , EndPhase CombatPhase
      ]
    pure g
  BeginPhase PlanningPhase -> do
    players <- getPlayers
    pushAll [WithTarget (toTarget player) (DoPhase PlanningPhase) | player <- players]
    pure g
  PlayAlly pid card -> do
    player <- getPlayer pid
    thisAlly <- newAlly card
    send $ Log $ player.name <> " plays " <> display (cdName $ toCardDef card)
    pushAll [RunWindow $ Window After $ EnterPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity (thisAlly & placementL .~ Controlled pid)
  AddToStaging card -> do
    case cdCardType (toCardDef card) of
      EnemyType -> do
        thisEnemy <- newEnemy card
        pure $ g & entitiesL . enemiesL %~ insertEntity (thisEnemy & placementL .~ Staging)
      LocationType -> do
        thisLocation <- newLocation card
        pure $ g & entitiesL . locationsL %~ insertEntity (thisLocation & placementL .~ Staging)
      _ -> error "Unhandled"
  _ -> pure g

getWindowAbilities :: (HasGame m) => Window -> m [Ability]
getWindowAbilities window = do
  abilities <- concatMap getAbilities . Map.elems <$> getsGame (.entities.allies)
  filterM
    ( andM
        . sequence
          [ pure . maybe False (windowMatches window) . abilityWindow
          , validCriteria . (.criteria)
          ]
    )
    abilities

validCriteria :: (HasGame m) => Criteria -> m Bool
validCriteria = \case
  LocationExists lMatcher -> selectAny lMatcher

runPlayerMessage :: Message -> Player -> GameT Player
runPlayerMessage msg player = case msg of
  RunWindow window -> do
    abilities <- getWindowAbilities window
    when (notNull abilities) $ do
      let (forced, nonForced) = partition isForcedAbility abilities
      if null forced
        then push $ Ask player.id $ ChooseOne $ map UseAbilityLabel nonForced
        else push $ Ask player.id $ ChooseOneAtATime $ map UseAbilityLabel forced
    pure player
  Ask pid q | pid == player.id -> do
    pure $ player {question = Just q}
  DoStep 1 Setup -> do
    deck <- shuffleM player.deck
    pure $ player {deck}
  DoStep 2 Setup -> do
    heroes <- select $ PlayerHero player.id
    let threat = sum $ map (.threat) heroes
    pure $ player {LordOfTheRings.Game.Base.threat = threat}
  DoStep 5 Setup -> do
    let (hand, deck) = splitAt 6 player.deck
    pure $ player {hand, deck}
  BeginPhase ResourcePhase -> do
    push $ DrawCards $ Drawing player.id 1
    pure player
  DrawCards drawing | drawing.player == player.id -> do
    let (cards, deck) = splitAt drawing.count player.deck
    pure $ player {hand = player.hand <> cards, deck}
  WithTarget (isTarget player -> True) (DoPhase PlanningPhase) -> do
    playable <- filterM (canPlay player) player.hand
    if null playable
      then pure player
      else
        pure $
          player
            { question =
                Just $
                  ChooseOne $
                    Label "Done Planning" []
                      : [ targetChoice c [PlayCard player.id c.id, WithTarget (toTarget player) (DoPhase PlanningPhase)]
                        | c <- playable
                        ]
            }
  PlayCard pid cid | pid == player.id -> do
    let mcard = find ((== cid) . (.id)) player.hand
    case mcard of
      Nothing -> error "not able to find card"
      Just playingCard -> do
        let hand = filter ((/= cid) . (.id)) player.hand
        costId <- getRandom
        let cost =
              ActiveCost
                { id = costId
                , cost =
                    ResourceCost (fromMaybe Neutral playingCard.sphere) (fromMaybe (StaticCost 0) playingCard.cost)
                , target = CostCardTarget playingCard
                , player = player.id
                }
        push $ PayCost costId
        pure $ player {hand, Player.cost = Just cost}
  SpendResources pid sphere n | pid == player.id && n > 0 -> do
    heroes <- select $ PlayerHero player.id
    let sphereMatches h = sphere == Neutral || h.sphere == sphere
    let heroesWithResources = filter (\h -> sphereMatches h && h.resources > 0) heroes
    case heroesWithResources of
      [x] -> do
        push $ SpendHeroResources x.id n
        pure player
      xs ->
        pure $
          player
            { question =
                Just
                  ( ChooseOne [targetChoice x [SpendHeroResources x.id 1, SpendResources pid sphere (n - 1)] | x <- xs]
                  )
            }
  PaidCost acId
    | Just cost <- player.cost
    , cost.id == acId -> do
        pure $ player {Player.cost = Nothing}
  _ -> pure player

instance RunMessage ActiveCost where
  runMessage msg ac = case msg of
    PayCost acId | acId == ac.id -> do
      case ac.cost of
        Free -> do
          push $ PaidCost ac.id
          pure ac
        ResourceCost sphere rCost -> case rCost of
          DynamicCost -> pure ac
          StaticCost n -> do
            pushAll [SpendResources ac.player sphere n, PayCost ac.id]
            pure ac {Cost.cost = Free}
    PaidCost acId | acId == ac.id -> do
      case ac.target of
        CostCardTarget (PlayerCard card) -> case cdCardType (toCardDef card) of
          AllyType -> push $ PlayAlly ac.player card
          _ -> pure ()
        _ -> error "Unhandled cost target"
      pure ac
    _ -> pure ac

instance RunMessage Player where
  runMessage msg player = do
    cost <- traverse (runMessage msg) player.cost
    runPlayerMessage msg $ player {Player.cost = cost}

canPlay :: (HasGame m) => Player -> Card -> m Bool
canPlay p c = case c.cost of
  Nothing -> pure False
  Just cost -> do
    andM
      [ maybe (pure False) (\s -> (>= costAsResources cost) <$> resourcesInSphere s p) c.sphere
      , passesUnique c
      , correctPhase c
      ]

passesUnique :: (HasGame m) => Card -> m Bool
passesUnique card = do
  if card.unique
    then andM [selectNone $ AllyWithName card.name, selectNone $ HeroWithName card.name]
    else pure True

correctPhase :: (HasGame m) => Card -> m Bool
correctPhase (PlayerCard card) = do
  case cdCardType (toCardDef card) of
    AllyType -> isPhase PlanningPhase
    AttachmentType -> isPhase PlanningPhase
    _ -> pure True
correctPhase (EncounterCard _) = pure False

isPhase :: (HasGame m) => Phase -> m Bool
isPhase phase = (== phase) <$> getPhase

getPhase :: (HasGame m) => m Phase
getPhase = getsGame (.scenario.phase)

resourcesInSphere :: (HasGame m) => Sphere -> Player -> m Int
resourcesInSphere sphere player = do
  heroes <- select $ PlayerHero player.id
  pure $ sum . map (.resources) $ filter (\h -> sphere == Neutral || h.sphere == sphere) heroes
