{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module LordOfTheRings.Game.Base) where

import Control.Monad.Random
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.IORef
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Game.Base
import LordOfTheRings.Game.Base as Cost (ActiveCost (..))
import LordOfTheRings.Game.Base as Player (Player (..))
import LordOfTheRings.Hero ()
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Queue
import LordOfTheRings.Run
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
  }

newtype GameT a = GameT {unGameT :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

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
getPlayers = Map.elems . (.players) <$> getGame

instance Run Game where
  run msg game = do
    players' <- traverse (run msg) game.players
    runGameMessage msg (game {players = players'})

runGameMessage :: (HasQueue m, HasGame m) => Message -> Game -> m Game
runGameMessage msg g = case msg of
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
  _ -> pure g

runPlayerMessage :: (MonadRandom m, HasQueue m) => Message -> Player -> m Player
runPlayerMessage msg player = case msg of
  Setup -> do
    (hand, deck) <- splitAt 6 <$> shuffleM player.deck
    let threat = sum $ map (.threat) player.heroes
    pure $ player {hand, deck, LordOfTheRings.Game.Base.threat = threat}
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
    let sphereMatches h = sphere == Neutral || h.sphere == sphere
    let heroesWithResources = filter (\h -> sphereMatches h && h.resources > 0) player.heroes
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
  PlayAlly pid cid code | pid == player.id -> do
    let allyCard = Map.findWithDefault (error "no ally") code allAllies
    thisAlly <- newAlly (Card cid allyCard)
    pure $ player {allies = Map.insert thisAlly.id thisAlly player.allies}
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
        CostCardTarget (SomeCard card@(Card _ def)) -> case def.kind of
          SAllyKind -> push $ PlayAlly ac.player card.id card.code
          _ -> pure ()
      pure ac
    _ -> pure ac

instance Run Player where
  run msg player = do
    heroes <- traverse (run msg) player.heroes
    cost <- traverse (run msg) player.cost
    runPlayerMessage msg $ player {LordOfTheRings.Game.Base.heroes = heroes, Player.cost = cost}

canPlay :: (Applicative m) => Player -> SomeCard -> m Bool
canPlay p c = case cardCost c of
  Nothing -> pure False
  Just cost -> pure $ resourcesInSphere c.sphere p >= costAsResources cost

resourcesInSphere :: Sphere -> Player -> Int
resourcesInSphere sphere player = sum . map (.resources) $ filter ((== sphere) . (.sphere)) player.heroes
