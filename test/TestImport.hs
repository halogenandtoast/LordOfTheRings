module TestImport (
  module X,
  module TestImport,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame as X
import LordOfTheRings.Classes.HasQueue as X
import LordOfTheRings.Card as X (Card (..), CardCode, CardDef, EncounterCard, genEncounterCard, genPlayerCard, toCardCode)
import LordOfTheRings.Classes.Entity as X (overAttrs, toAttrs, toId)
import LordOfTheRings.Scenario.Types (encounterDeckL)
import LordOfTheRings.Decklist as X (DeckList (..), starterDeck)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Hero as X (Hero, HeroAttrs (..))
import LordOfTheRings.Quest as X (Quest, QuestAttrs (..))
import LordOfTheRings.Source as X
import LordOfTheRings.Token as X
import LordOfTheRings.Game as X
import LordOfTheRings.GameT as X
import LordOfTheRings.Id as X
import LordOfTheRings.Matcher as X (getPlayer)
import LordOfTheRings.Message as X
import LordOfTheRings.Prelude as X
import LordOfTheRings.Question as X
import LordOfTheRings.Target as X
import Test.Hspec as X hiding (shouldBe, shouldReturn, shouldSatisfy)
import Test.Hspec.Expectations.Lifted as X (shouldBe, shouldReturn, shouldSatisfy)

-- | Run a test against a one player game built from the starter deck.
gameTest :: (PlayerId -> GameT ()) -> IO ()
gameTest = gameTestWith testDeck

gameTestWith :: DeckList -> (PlayerId -> GameT ()) -> IO ()
gameTestWith deck body = do
  (player, heroes) <- either (error . show) pure =<< loadPlayer deck
  env <- newGameEnv (newGame player heroes) (const $ pure ()) 1
  runGameT env (body player.id)

-- | Beravor, Éowyn and Théodred (27 starting threat) with a mix of allies, attachments and events.
testDeck :: DeckList
testDeck =
  DeckList
    ["01012", "01007", "01002"]
    [ ("01013", 3), ("01014", 2), ("01015", 2), ("01016", 3), ("01017", 2), ("01031", 1)
    , ("01045", 2), ("01059", 2), ("01060", 1), ("01061", 2), ("01062", 2), ("01073", 3)
    , ("01026", 2), ("01027", 1), ("01057", 1), ("01069", 2), ("01070", 2), ("01072", 2)
    , ("01020", 2), ("01023", 2), ("01025", 1), ("01046", 2), ("01048", 2), ("01050", 2)
    , ("01051", 2), ("01053", 1), ("01064", 1)
    ]

-- | Push a message and run the queue until it empties or a question is asked.
run :: Message -> GameT ()
run msg = push msg >> void mainLoop

getQuestion :: PlayerId -> GameT (Maybe (Question Message))
getQuestion pid = Map.lookup pid <$> getsGame (.question)

-- | Answer the open question with the first choice that matches.
chooseOptionMatching :: (HasCallStack) => Text -> PlayerId -> (UI Message -> Bool) -> GameT ()
chooseOptionMatching reason pid f = do
  question <- maybe (error $ "no question when trying to " <> unpack reason) pure =<< getQuestion pid
  case findIndex f (questionChoices question) of
    Nothing -> error $ "could not " <> unpack reason
    Just n -> void $ answerQuestion pid n

chooseLabel :: (HasCallStack) => Text -> PlayerId -> GameT ()
chooseLabel l pid = chooseOptionMatching ("choose " <> l) pid \case
  Label l' _ -> l == l'
  _ -> False

chooseTarget :: (HasCallStack, Targetable a) => PlayerId -> a -> GameT ()
chooseTarget pid (toTarget -> target) = chooseOptionMatching "choose target" pid \case
  TargetLabel target' _ -> target == target'
  _ -> False

putCardIntoHand :: PlayerId -> CardDef -> GameT Card
putCardIntoHand pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . handL %~ (card :)
  pure card

getHeroWithCode :: (HasCallStack) => CardCode -> GameT Hero
getHeroWithCode code =
  fromJustNote ("no hero " <> show code) . find ((== code) . toCardCode) . toList <$> getsGame (.entities.heroes)

getHeroResources :: HeroId -> GameT Int
getHeroResources hid = maybe 0 (.resources) . Map.lookup hid <$> getsGame (.entities.heroes)

chooseAbility :: (HasCallStack) => PlayerId -> GameT ()
chooseAbility pid = chooseOptionMatching "use ability" pid \case
  AbilityLabel {} -> True
  _ -> False

flipFirstQuest :: PlayerId -> GameT ()
flipFirstQuest pid = do
  run Setup
  chooseLabel "Keep this hand" pid
  chooseOptionMatching "flip the quest" pid (const True)

-- | Process exactly one message, leaving whatever it queues unprocessed.
runOnly :: Message -> GameT ()
runOnly msg = push msg >> void runNext

getActiveQuest :: (HasCallStack) => GameT Quest
getActiveQuest = getsGame (fromJustNote "no active quest" . headMay . toList . (.entities.quests))

setEncounterDeck :: [CardDef] -> GameT [EncounterCard]
setEncounterDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDeckL .~ cards)
  pure cards

getEncounterDiscard :: GameT [EncounterCard]
getEncounterDiscard = getsGame ((.encounterDiscard) . toAttrs . (.scenario))

getEncounterDeck :: GameT [EncounterCard]
getEncounterDeck = getsGame ((.encounterDeck) . toAttrs . (.scenario))
