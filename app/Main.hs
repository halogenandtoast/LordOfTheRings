{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Conduit
import Control.Monad (forever)
import Control.Monad.Extra (concatMapM, replicateM)
import Control.Monad.Random (
  MonadRandom (..),
  getRandom,
  newStdGen,
  randomIO,
  randomRIO,
  randomRs,
  randoms,
 )
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.Map.Strict qualified as Map
import GHC.Generics
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Game
import LordOfTheRings.Game qualified as E
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Queue
import LordOfTheRings.Scenario
import UnliftIO.STM
import Yesod.Core
import Yesod.WebSockets

data GameData = GameData
  { game :: Game
  , playerId :: PlayerId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

allAttachments :: Map CardCode (CardDef 'AttachmentKind)
allAttachments =
  Map.fromList
    [ ("01026", stewardOfGondor)
    , ("01027", celebriansStone)
    , ("01057", unexpectedCourage)
    , ("01069", forestSnare)
    , ("01070", protectorOfLorien)
    , ("01072", selfPreservation)
    ]

attachment :: CardCode -> Name -> Sphere -> Int -> CardDef 'AttachmentKind
attachment code name sphere cost =
  CardDef
    { code
    , name
    , unique = False
    , kind = SAttachmentKind
    , details = AttachmentDetails (StaticCost cost) False
    , sphere
    }

restricted :: CardDef 'AttachmentKind -> CardDef 'AttachmentKind
restricted def = def {details = def.details {restricted = True}}

stewardOfGondor :: CardDef 'AttachmentKind
stewardOfGondor = unique $ attachment "01026" "Steward of Gondor" Leadership 2

celebriansStone :: CardDef 'AttachmentKind
celebriansStone = unique $ restricted $ attachment "01027" "Celebrian's Stone" Leadership 2

unexpectedCourage :: CardDef 'AttachmentKind
unexpectedCourage = attachment "01057" "Unexpected Courage" Spirit 2

forestSnare :: CardDef 'AttachmentKind
forestSnare = attachment "01069" "Forest Snare" Lore 3

protectorOfLorien :: CardDef 'AttachmentKind
protectorOfLorien = attachment "01070" "Protector of Lorien" Lore 1

selfPreservation :: CardDef 'AttachmentKind
selfPreservation = attachment "01072" "Self Preservation" Lore 3

allEvents :: Map CardCode (CardDef 'EventKind)
allEvents =
  Map.fromList
    [ ("01050", aTestOfWill)
    , ("01053", dwarvenTomb)
    , ("01020", everVigilant)
    , ("01025", grimResolve)
    , ("01048", hastyStroke)
    , ("01064", loriensWealth)
    , ("01023", sneakAttack)
    , ("01051", standAndFight)
    , ("01046", theGaladhrimsGreeting)
    ]

event :: CardCode -> Name -> Sphere -> Int -> CardDef 'EventKind
event code name sphere cost =
  CardDef
    { code
    , name
    , unique = False
    , kind = SEventKind
    , details = EventDetails (StaticCost cost)
    , sphere
    }

aTestOfWill :: CardDef 'EventKind
aTestOfWill = event "01050" "A Test of Will" Spirit 1

dwarvenTomb :: CardDef 'EventKind
dwarvenTomb = event "01053" "Dwarven Tomb" Spirit 1

everVigilant :: CardDef 'EventKind
everVigilant = event "01020" "Ever Vigilant" Leadership 1

grimResolve :: CardDef 'EventKind
grimResolve = event "01025" "Grim Resolve" Leadership 5

hastyStroke :: CardDef 'EventKind
hastyStroke = event "01048" "Hasty Stroke" Spirit 1

loriensWealth :: CardDef 'EventKind
loriensWealth = event "01064" "Lorien's Wealth" Lore 3

sneakAttack :: CardDef 'EventKind
sneakAttack = event "01023" "Sneak Attack" Leadership 1

standAndFight :: CardDef 'EventKind
standAndFight = dynamic $ event "01051" "Stand and Fight" Spirit 0

theGaladhrimsGreeting :: CardDef 'EventKind
theGaladhrimsGreeting = event "01046" "The Galadhrim's Greeting" Spirit 3

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

data Deck = Deck
  { heroes :: [Card 'HeroKind]
  , cards :: [SomeCard]
  }
  deriving stock (Show)

newtype DeckException = DeckException Text
  deriving newtype (Show)

loadPlayer :: (MonadRandom m) => DeckList -> m (Either DeckException (Player, [Hero]))
loadPlayer dl = traverse go =<< loadDecklist dl
 where
  go deck = do
    pid <- getRandom
    heroes <- traverse (newHero pid) deck.heroes
    pure $
      ( Player
          { id = pid
          , deck = map (setCardOwner pid) deck.cards
          , hand = []
          , threat = 0
          , question = Nothing
          , cost = Nothing
          , name = "halogen64"
          }
      , heroes
      )

setCardOwner :: PlayerId -> SomeCard -> SomeCard
setCardOwner pid (SomeCard c) = SomeCard $ c {owner = Just pid}

missingCard :: CardCode -> DeckException
missingCard code = DeckException $ "Missing card: " <> tshow code

allCardDefs :: Map CardCode SomeCardDef
allCardDefs =
  Map.map SomeCardDef allHeroes
    <> Map.map SomeCardDef allAllies
    <> Map.map SomeCardDef allAttachments
    <> Map.map SomeCardDef allEvents

lookupCard :: (MonadRandom m) => CardCode -> m (Either DeckException SomeCard)
lookupCard code = case Map.lookup code allCardDefs of
  Nothing -> pure $ Left $ missingCard code
  Just (SomeCardDef c) -> Right . SomeCard <$> newCard c Nothing

lookupHeroCard :: (MonadRandom m) => CardCode -> m (Either DeckException (Card 'HeroKind))
lookupHeroCard code = case Map.lookup code allHeroes of
  Nothing -> pure $ Left $ missingCard code
  Just c -> Right <$> newCard c Nothing

loadDecklist :: (MonadRandom m) => DeckList -> m (Either DeckException Deck)
loadDecklist DeckList {heroes, cards} = do
  loadedHeroes <- sequence <$> traverse lookupHeroCard heroes
  loadedCards <- sequence <$> concatMapM (\(code, n) -> replicateM n (lookupCard code)) cards
  pure $ Deck <$> loadedHeroes <*> loadedCards

data Room = Room
  { env :: Env
  , chan :: TChan ByteString
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
  g <- liftIO $ runGameT mainLoop room.env
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
  g <- liftIO $ flip runGameT room.env $ do
    clearQuestion response.player
    pushAll $ choiceMessages response.choice
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
  liftIO $ flip runGameT room.env $ do
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
          , entities = mempty {E.heroes = Map.fromList [(h.id, h) | h <- heroes]}
          , scenario = newScenario PassageThroughMirkwood
          }

main :: IO ()
main = do
  ePlayer <- loadPlayer testDeckList
  case ePlayer of
    Left err -> error (show err)
    Right (player, heroes) -> liftIO $ do
      queue <- newQueue
      game <-
        newIORef $
          Game
            { players = Map.singleton player.id player
            , activePlayer = player.id
            , entities = mempty {E.heroes = Map.fromList [(h.id, h) | h <- heroes]}
            , scenario = newScenario PassageThroughMirkwood
            }
      c <- atomically newTChan
      let
        handler msg = atomically $ writeTChan c (encode $ toJSON msg)
      let env = Env {queue, game, handler}
      let room = Room {env, chan = c}
      runGameT (pushAll [Setup, BeginRound, EndRound]) env
      warp 3000 (App room)
