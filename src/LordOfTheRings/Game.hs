{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module LordOfTheRings.Game.Base) where

import Control.Monad.Random (MonadRandom (..), newStdGen, randomIO, randomRIO, randomRs, randoms)
import Control.Monad.Reader
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Enemy
import LordOfTheRings.Entity
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base
import LordOfTheRings.Game.Base as Cost (ActiveCost (..))
import LordOfTheRings.Game.Base as Player (Player (..))
import LordOfTheRings.Hero ()
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
import LordOfTheRings.Run
import LordOfTheRings.Scenario ()
import LordOfTheRings.Window

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
      g' <- run msg g
      putGame g'

      -- do we have a question
      hasQuestion <- getHasQuestion
      if hasQuestion
        then pure g'
        else mainLoop

instance MonadRandom GameT where
  getRandom = liftIO randomIO
  getRandomR = liftIO . randomRIO
  getRandomRs lohi = fmap (randomRs lohi) newStdGen
  getRandoms = fmap randoms newStdGen

data Env = Env
  { queue :: Queue Message
  , game :: IORef Game
  , handler :: FrontendMessage -> IO ()
  , debug :: Int
  }

class HasDebug m where
  getDebug :: m Int

instance HasDebug GameT where
  getDebug = asks (.debug)

debug :: (HasDebug m, MonadIO m) => Int -> IO () -> m ()
debug lvl action = do
  lvl' <- getDebug
  when (lvl' >= lvl) $ liftIO action

newtype GameT a = GameT {unGameT :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance Frontend GameT where
  send msg = asks (.handler) >>= \f -> liftIO (f msg)

runGameT :: GameT a -> Env -> IO a
runGameT body = runReaderT body.unGameT

putGame :: Game -> GameT ()
putGame game = do
  ref <- asks (.game)
  liftIO $ writeIORef ref game

instance HasGame GameT where
  getGame = asks (.game) >>= liftIO . readIORef

instance HasQueue Message GameT where
  messageQueue = asks (.queue)

getPlayers :: (HasGame m) => m [Player]
getPlayers = Map.elems <$> getsGame (.players)

instance Run Entities where
  run msg entities = do
    heroes <- traverse (run msg) entities.heroes
    allies <- traverse (run msg) entities.allies
    locations <- traverse (run msg) entities.locations
    enemies <- traverse (run msg) entities.enemies
    pure (entities {heroes, allies, locations, enemies})

instance Run Game where
  run msg game = do
    players <- traverse (run msg) game.players
    entities <- run msg game.entities
    scenario <- run msg game.scenario
    runGameMessage msg (game {players, entities, scenario})

runGameMessage :: (HasQueue Message m, HasGame m, MonadRandom m, Frontend m) => Message -> Game -> m Game
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
    send $ Log $ player.name <> " plays " <> display card.name
    pushAll [RunWindow $ Window After $ EnterPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity (thisAlly & placementL .~ Controlled pid)
  AddToStaging (SomeEncounterCard card) -> do
    case card.def.kind of
      SEnemyKind -> do
        thisEnemy <- newEnemy card
        pure $ g & entitiesL . enemiesL %~ insertEntity (thisEnemy & placementL .~ Staging)
      SLocationKind -> do
        thisLocation <- newLocation card
        pure $ g & entitiesL . locationsL %~ insertEntity (thisLocation & placementL .~ Staging)
      _ -> error "Unhandled"
  _ -> pure g

instance Entity Ally where
  type Id Ally = AllyId
  type Attrs Ally = AllyAttrs
  overAttrs f (Ally a) = Ally (overAllyAttrs f a)

instance Entity Enemy where
  type Id Enemy = EnemyId
  type Attrs Enemy = EnemyAttrs
  overAttrs f (Enemy a) = Enemy (overEnemyAttrs f a)

instance Entity Location where
  type Id Location = LocationId
  type Attrs Location = LocationAttrs
  overAttrs f (Location a) = Location (overLocationAttrs f a)

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

runPlayerMessage :: (MonadRandom m, HasQueue Message m, HasGame m) => Message -> Player -> m Player
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

instance Run ActiveCost where
  run msg ac = case msg of
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
        CostCardTarget (PlayerCard (SomePlayerCard card)) -> case card.def.kind of
          SAllyKind -> push $ PlayAlly ac.player card
          _ -> pure ()
        _ -> error "Unhandled cost target"
      pure ac
    _ -> pure ac

instance Run Player where
  run msg player = do
    cost <- traverse (run msg) player.cost
    runPlayerMessage msg $ player {Player.cost = cost}

canPlay :: (HasGame m) => Player -> Card -> m Bool
canPlay p c = case cardCost c of
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
correctPhase (PlayerCard (SomePlayerCard card)) = do
  case card.def.kind of
    SAllyKind -> isPhase PlanningPhase
    SAttachmentKind -> isPhase PlanningPhase
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
