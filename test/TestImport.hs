module TestImport (
  module X,
  module TestImport,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame as X
import LordOfTheRings.Classes.HasQueue as X
import LordOfTheRings.Card as X (Card (..), CardCode, CardDef, EncounterCard, genEncounterCard, genPlayerCard, toCardCode)
import LordOfTheRings.Classes.Entity as X (insertEntity, overAttrs, toAttrs, toId)
import LordOfTheRings.Scenario.Types (encounterDeckL)
import LordOfTheRings.Scenario as X (Scenario, escapeFromDolGuldur, journeyAlongTheAnduin, passageThroughMirkwood)
import LordOfTheRings.Decklist as X (DeckList (..), coreStarterDecks, starterDeck)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Hero as X (Hero, HeroAttrs (..))
import LordOfTheRings.Ally as X (AllyAttrs (..))
import LordOfTheRings.Attachment (AttachmentAttrs (..), newAttachment)

import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.Entities (attachmentsL)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement as X (Placement (..))
import LordOfTheRings.Quest as X (Quest, QuestAttrs (..))
import LordOfTheRings.Source as X
import LordOfTheRings.Ability.Types as X (Ability (..))
import LordOfTheRings.Token as X
import LordOfTheRings.Game as X
import LordOfTheRings.GameT as X
import LordOfTheRings.Id as X
import LordOfTheRings.Matcher as X (getDamage, getLocation, getPlayer)
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
  (player, heroes) <- either (error . show) pure =<< loadPlayer "tester" deck
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

-- | Use the offered ability of the given card.
chooseAbilityOf :: (HasCallStack, Targetable a) => PlayerId -> a -> GameT ()
chooseAbilityOf pid (toTarget -> target) = chooseOptionMatching "use the card's ability" pid \case
  AbilityLabel _ ab _ _ -> sourceTarget ab.abilitySource == Just target
  _ -> False

-- | Whether the open question offers an ability of the given card.
offersAbilityOf :: (Targetable a) => PlayerId -> a -> GameT Bool
offersAbilityOf pid (toTarget -> target) =
  maybe False (any \case AbilityLabel _ ab _ _ -> sourceTarget ab.abilitySource == Just target; _ -> False) . fmap questionChoices
    <$> getQuestion pid

-- | One of the core set's sphere starter decks.
coreDeck :: Text -> DeckList
coreDeck sphere = fromJustNote "no such starter deck" $ lookup sphere coreStarterDecks

getCharacterDamage :: Target -> GameT Int
getCharacterDamage = getDamage

-- | Decline any open action window (planning or otherwise), for tests about what follows.
passActions :: PlayerId -> GameT ()
passActions pid =
  getQuestion pid >>= \case
    Just (QuestionLabel prompt _ _ q) | prompt `elem` ["Take an action?", "Play cards or use actions"] ->
      case findIndex (\case Label l _ -> l `elem` ["Pass", "Done Planning"]; _ -> False) (questionChoices q) of
        Just n -> answerQuestion pid n >> passActions pid
        Nothing -> pure ()
    _ -> pure ()

-- | Put an ally from hand into play under the player's control, returning it as a target.
-- Any response to it entering play is left open.
putAllyIntoPlay :: PlayerId -> CardDef -> GameT Target
putAllyIntoPlay pid def = do
  card <- putCardIntoHand pid def
  run $ PutAllyIntoPlay pid card.id
  getsGame (toTarget . fromJustNote "ally did not enter play" . find ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies))

-- | An attachment of the player's put straight onto a host, skipping play and payment.
putAttachmentOn :: PlayerId -> CardDef -> Target -> GameT Target
putAttachmentOn pid def host = do
  card <- genPlayerCard def (Just pid)
  att <- overAttrs (\a -> a {attachmentPlacement = AttachedTo host}) <$> newAttachment pid card
  g <- getGame
  putGame =<< preloadModifiers (g & entitiesL . attachmentsL %~ insertEntity att)
  pure $ toTarget att

-- | Give a hero resources, then play a card from hand in the planning phase, paid from
-- that hero alone.
playWith :: PlayerId -> CardCode -> Int -> CardDef -> GameT Card
playWith pid heroCode resources def = do
  card <- putCardIntoHand pid def
  payer <- getHeroWithCode heroCode
  run $ PlaceTokens GameSource (toTarget payer) Resource resources
  run $ BeginPhase PlanningPhase
  chooseTarget pid (CardIdTarget card.id)
  pure card

-- | Run a test against a one player game of the given scenario.
gameTestIn :: Scenario -> DeckList -> (PlayerId -> GameT ()) -> IO ()
gameTestIn scenario deck body = do
  (player, heroes) <- either (error . show) pure =<< loadPlayer "tester" deck
  env <- newGameEnv (newGameIn scenario player heroes) (const $ pure ()) 1
  runGameT env (body player.id)

-- | Put a quest stage straight into play, with nothing else in the quest deck.
putQuestStage :: CardDef -> GameT Target
putQuestStage def = do
  card <- genEncounterCard def
  run $ PutQuestIntoPlay card
  toTarget <$> getActiveQuest
