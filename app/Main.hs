{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

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
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Game
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Queue
import Yesod.Core

data GameData = GameData
  { game :: Game
  , playerId :: PlayerId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

allAllies :: Map CardCode (CardDef 'AllyKind)
allAllies =
  Map.fromList
    [ ("01013", guardOfTheCitadel)
    , ("01014", faramir)
    , ("01015", sonOfArnor)
    , ("01016", snowbournScout)
    , ("01017", silverlodeArcher)
    , ("01031", beorn)
    , ("01045", northernTracker)
    , ("01059", ereborHammersmith)
    , ("01060", henamarthRiversong)
    , ("01061", minerOfTheIronHills)
    , ("01062", gleowine)
    , ("01073", gandalf)
    ]

ally :: CardCode -> Name -> Sphere -> Int -> Int -> Int -> Int -> Int -> CardDef 'AllyKind
ally code name sphere cost willpower attack defense hitPoints =
  CardDef
    { code
    , name
    , unique = False
    , kind = SAllyKind
    , details = AllyDetails (StaticCost cost) willpower attack defense hitPoints
    , sphere
    }

unique :: CardDef k -> CardDef k
unique def = def {unique = True}

class CanBeDynamic a where
  dynamic :: a -> a

instance CanBeDynamic (CardDef 'EventKind) where
  dynamic def = def {details = EventDetails DynamicCost}

beorn :: CardDef 'AllyKind
beorn = unique $ ally "01031" "Beorn" Tactics 6 1 3 3 6

ereborHammersmith :: CardDef 'AllyKind
ereborHammersmith = ally "01059" "Erebor Hammersmith" Lore 2 1 1 1 3

faramir :: CardDef 'AllyKind
faramir = unique $ ally "01014" "Faramir" Leadership 4 2 1 2 3

gandalf :: CardDef 'AllyKind
gandalf = unique $ ally "01073" "Gandalf" Neutral 5 4 4 4 4

gleowine :: CardDef 'AllyKind
gleowine = unique $ ally "01062" "Gleowine" Lore 2 1 0 0 2

guardOfTheCitadel :: CardDef 'AllyKind
guardOfTheCitadel = ally "01013" "Guard of the Citadel" Leadership 2 1 1 0 2

henamarthRiversong :: CardDef 'AllyKind
henamarthRiversong = unique $ ally "01060" "Henamarth Riversong" Lore 1 1 1 0 1

minerOfTheIronHills :: CardDef 'AllyKind
minerOfTheIronHills = ally "01061" "Miner of the Iron Hills" Lore 2 0 0 1 2

northernTracker :: CardDef 'AllyKind
northernTracker = ally "01045" "Nothern Tracker" Spirit 4 1 2 2 3

silverlodeArcher :: CardDef 'AllyKind
silverlodeArcher = ally "01017" "Silverload Archer" Leadership 3 1 2 0 1

snowbournScout :: CardDef 'AllyKind
snowbournScout = ally "01016" "Snowbourn Scout" Leadership 1 0 0 1 1

sonOfArnor :: CardDef 'AllyKind
sonOfArnor = ally "01015" "Son of Arnor" Leadership 3 0 2 0 2

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

newCard :: (MonadRandom m) => CardDef k -> m (Card k)
newCard def = Card <$> getRandom <*> pure def

loadPlayer :: (MonadRandom m) => DeckList -> m (Either DeckException Player)
loadPlayer dl = traverse go =<< loadDecklist dl
 where
  go deck = do
    pid <- getRandom
    heroes <- traverse newHero deck.heroes
    pure $
      Player
        { id = pid
        , heroes
        , deck = deck.cards
        , hand = []
        , threat = 0
        , question = Nothing
        , cost = Nothing
        }

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
  Just (SomeCardDef c) -> Right . SomeCard <$> newCard c

lookupHeroCard :: (MonadRandom m) => CardCode -> m (Either DeckException (Card 'HeroKind))
lookupHeroCard code = case Map.lookup code allHeroes of
  Nothing -> pure $ Left $ missingCard code
  Just c -> Right <$> newCard c

loadDecklist :: (MonadRandom m) => DeckList -> m (Either DeckException Deck)
loadDecklist DeckList {heroes, cards} = do
  loadedHeroes <- sequence <$> traverse lookupHeroCard heroes
  loadedCards <- sequence <$> concatMapM (\(code, n) -> replicateM n (lookupCard code)) cards
  pure $ Deck <$> loadedHeroes <*> loadedCards

data App = App {env :: Env}

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

getGameR :: Handler GameData
getGameR = do
  App {env} <- getYesod
  g <- liftIO $ runGameT mainLoop env
  pure $ GameData g g.activePlayer

postGameR :: Handler GameData
postGameR = do
  App {env} <- getYesod
  choice <- requireCheckJsonBody
  liftIO $ do
    g <- flip runGameT env $ do
      pushAll $ choiceMessages choice
      mainLoop
    pure $ GameData g g.activePlayer

postResetR :: Handler GameData
postResetR = do
  App {env} <- getYesod
  liftIO $ flip runGameT env $ do
    clearQueue
    resetGame
    pushAll [Setup, BeginRound, EndRound]
    g <- mainLoop
    pure $ GameData g g.activePlayer

resetGame :: GameT ()
resetGame = do
  ePlayer <- loadPlayer testDeckList
  case ePlayer of
    Left err -> error (show err)
    Right player -> putGame $ Game (Map.singleton player.id player) player.id

main :: IO ()
main = do
  ePlayer <- loadPlayer testDeckList
  case ePlayer of
    Left err -> error (show err)
    Right player -> liftIO $ do
      queue <- newQueue
      game <- newIORef $ Game (Map.singleton player.id player) player.id
      let env = Env {queue, game}
      runGameT (pushAll [Setup, BeginRound, EndRound]) env
      warp 3000 (App env)
