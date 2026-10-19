{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Conduit
import Control.Monad.Random (
  MonadRandom (..),
  getRandom,
  newStdGen,
  mkStdGen,
  randomIO,
  randomRIO,
  randomRs,
  randoms,
 )
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Game
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.GameT
import LordOfTheRings.Game qualified as E
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Name
import LordOfTheRings.Prelude hiding (Handler)
import LordOfTheRings.Question
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Scenario
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import UnliftIO.STM
import Yesod.Core
import Yesod.WebSockets

data GameData = GameData
  { game :: Game
  , playerId :: PlayerId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data DeckList = DeckList
  { heroes :: [CardCode]
  , cards :: [(CardCode, Int)]
  }

testDeckList :: DeckList
testDeckList =
  DeckList
    ["01012", "01007", "01002"]
    [ ("01031", 1)
    , ("01059", 2)
    , ("01014", 2)
    , ("01073", 3)
    , ("01062", 2)
    , ("01013", 3)
    , ("01060", 1)
    , ("01061", 2)
    , ("01045", 2)
    , ("01017", 2)
    , ("01016", 3)
    , ("01015", 2)
    , ("01027", 1)
    , ("01069", 2)
    , ("01070", 2)
    , ("01072", 2)
    , ("01026", 2)
    , ("01057", 1)
    , ("01050", 2)
    , ("01053", 1)
    , ("01020", 2)
    , ("01025", 1)
    , ("01048", 2)
    , ("01064", 1)
    , ("01023", 2)
    , ("01051", 2)
    , ("01046", 2)
    ]

newtype DeckException = DeckException Text
  deriving newtype (Show)

lookupPlayerCardDef :: CardCode -> Either DeckException CardDef
lookupPlayerCardDef cCode =
  maybe (Left $ DeckException $ "Missing card: " <> tshow cCode) Right $ Map.lookup cCode allPlayerCards

loadPlayer :: (MonadRandom m) => DeckList -> m (Either DeckException (Player, [Hero]))
loadPlayer dl = do
  pid <- getRandom
  let defs =
        (,)
          <$> traverse lookupPlayerCardDef dl.heroes
          <*> (concat <$> traverse (\(cCode, n) -> replicate n <$> lookupPlayerCardDef cCode) dl.cards)
  for defs \(heroDefs, cardDefs) -> do
    heroes <- traverse (newHero pid <=< (`genPlayerCard` Just pid)) heroDefs
    deck <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) cardDefs
    pure
      ( Player
          { id = pid
          , deck
          , hand = []
          , threat = 0
          , question = Nothing
          , cost = Nothing
          , name = "halogen64"
          }
      , heroes
      )

data Room = Room
  { env :: GameEnv
  , chan :: TChan LByteString
  }

data App = App {room :: Room}

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /data GameR GET POST
  /reset ResetR POST
|]

instance Yesod App

instance ToTypedContent GameData where
  toTypedContent = toTypedContent . toJSON

instance ToContent GameData where
  toContent = toContent . toJSON

instance MonadRandom (HandlerFor App) where
  getRandom = liftIO randomIO
  getRandomR = liftIO . randomRIO
  getRandomRs lohi = fmap (randomRs lohi) newStdGen
  getRandoms = fmap randoms newStdGen

handleSocket :: Room -> WebSocketsT Handler ()
handleSocket room = do
  readChannel <- atomically $ dupTChan room.chan
  race_
    (forever $ atomically (readTChan readChannel) >>= sendTextData)
    (runConduit $ sourceWS .| mapM_C (atomically . writeTChan room.chan))

getGameR :: Handler GameData
getGameR = do
  App {room} <- getYesod
  webSockets (handleSocket room)
  g <- liftIO $ runGameT room.env mainLoop
  pure $ GameData g g.activePlayer

data Response = Response
  { choice :: Choice Message
  , player :: PlayerId
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postGameR :: Handler ()
postGameR = do
  App {room} <- getYesod
  response <- requireCheckJsonBody @_ @Response
  g <- liftIO $ runGameT room.env $ do
    clearQuestion response.player
    pushAll $ choiceMessages response.player response.choice
    mainLoop

  liftIO $
    atomically $
      writeTChan room.chan $
        encode $
          toJSON $
            object ["contents" .= GameData g g.activePlayer, "tag" .= String "Update"]

clearQuestion :: PlayerId -> GameT ()
clearQuestion pid = do
  g <- getGame
  putGame $ g {players = Map.adjust (\p -> p {question = Nothing}) pid g.players}

postResetR :: Handler ()
postResetR = do
  App {room} <- getYesod
  liftIO $ runGameT room.env $ do
    clearQueue
    resetGame
    pushAll [Setup, BeginRound, EndRound]
    g <- mainLoop
    atomically $
      writeTChan room.chan $
        encode $
          toJSON $
            object ["contents" .= GameData g g.activePlayer, "tag" .= String "Update"]

resetGame :: GameT ()
resetGame = do
  ePlayer <- loadPlayer testDeckList
  case ePlayer of
    Left err -> error (show err)
    Right (player, heroes) ->
      putGame $
        Game
          { players = Map.singleton player.id player
          , activePlayer = player.id
          , firstPlayer = player.id
          , entities = mempty {E.heroes = Map.fromList [(h.id, h) | h <- heroes]}
          , scenario = newScenario PassageThroughMirkwood
          }

main :: IO ()
main = do
  ePlayer <- loadPlayer testDeckList
  case ePlayer of
    Left err -> error (show err)
    Right (player, heroes) -> liftIO $ do
      queue <- newQueue []
      game <-
        newIORef $
          Game
            { players = Map.singleton player.id player
            , activePlayer = player.id
            , firstPlayer = player.id
            , entities = mempty {E.heroes = Map.fromList [(h.id, h) | h <- heroes]}
            , scenario = newScenario PassageThroughMirkwood
            }
      c <- atomically newTChan
      let handler msg = atomically $ writeTChan c (encode $ toJSON msg)
      gen <- newIORef . mkStdGen =<< randomIO
      let env = GameEnv {gameEnvGame = game, gameEnvQueue = queue, gameRandomGen = gen, gameLogger = handler, gameDebug = 1}
      let room = Room {env, chan = c}
      runGameT env (pushAll [Setup, BeginRound, EndRound])
      port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PORT"
      warp port (App room)
