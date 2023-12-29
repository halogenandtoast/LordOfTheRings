{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module LordOfTheRings.Game.Base) where

import Control.Lens ((%~), (&), (.~))
import Control.Monad.Random
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.IORef
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Enemy
import LordOfTheRings.Entity
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base
import LordOfTheRings.Game.Base as Cost (ActiveCost (..))
import LordOfTheRings.Game.Base as Player (Player (..))
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Location
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Queue
import LordOfTheRings.Run
import LordOfTheRings.Scenario ()
import System.Random.Shuffle (shuffleM)

getHasQuestion :: GameT Bool
getHasQuestion = any (isJust . (.question)) <$> getPlayers

mainLoop :: GameT Game
mainLoop = do
  mmsg <- pop
  g <- getGame
  case mmsg of
    Nothing -> pure g
    Just msg -> do
      liftIO $ print msg
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
  { queue :: Queue
  , game :: IORef Game
  , handler :: FrontendMessage -> IO ()
  }

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

instance HasQueue GameT where
  getQueue = asks (.queue)

getPlayers :: (HasGame m) => m [Player]
getPlayers = Map.elems <$> getsGame (.players)

instance Run Entities where
  run msg entities = do
    heroes <- traverse (run msg) entities.heroes
    allies <- traverse (run msg) entities.allies
    pure (entities {heroes, allies})

instance Run Game where
  run msg game = do
    players <- traverse (run msg) game.players
    entities <- run msg game.entities
    scenario <- run msg game.scenario
    runGameMessage msg (game {players, entities, scenario})

runGameMessage :: (HasQueue m, HasGame m, MonadRandom m, Frontend m) => Message -> Game -> m Game
runGameMessage msg g = case msg of
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
    for_ players \player -> do
      push $ WithPlayer player.id msg
    pure g
  PlayAlly pid card -> do
    player <- getPlayer pid
    thisAlly <- newAlly card
    send $ Log $ player.name <> " plays " <> display card.name
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

runPlayerMessage :: (MonadRandom m, HasQueue m, HasGame m) => Message -> Player -> m Player
runPlayerMessage msg player = case msg of
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
    let (cards, deck) = splitAt 1 player.deck
    pure $ player {hand = player.hand <> cards, deck}
  WithPlayer pid (BeginPhase PlanningPhase) | pid == player.id -> do
    playable <- filterM (canPlay player) player.hand
    if null playable
      then pure player
      else
        pure $
          player {question = Just (ChooseOne [CardChoice c.id [PlayCard player.id c.id] | c <- playable])}
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
                , cost = ResourceCost playingCard.sphere (fromMaybe (StaticCost 0) playingCard.cost)
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
                  ( ChooseOne [HeroChoice x.id [SpendHeroResources x.id 1, SpendResources pid sphere (n - 1)] | x <- xs]
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
        CostCardTarget (SomeCard card) -> case card.def.kind of
          SAllyKind -> push $ PlayAlly ac.player card
          _ -> pure ()
      pure ac
    _ -> pure ac

instance Run Player where
  run msg player = do
    cost <- traverse (run msg) player.cost
    runPlayerMessage msg $ player {Player.cost = cost}

canPlay :: (HasGame m) => Player -> SomeCard -> m Bool
canPlay p c = case cardCost c of
  Nothing -> pure False
  Just cost -> do
    resources <- resourcesInSphere c.sphere p
    pure $ resources >= costAsResources cost

resourcesInSphere :: (HasGame m) => Sphere -> Player -> m Int
resourcesInSphere sphere player = do
  heroes <- select $ PlayerHero player.id
  pure $ sum . map (.resources) $ filter ((== sphere) . (.sphere)) heroes
