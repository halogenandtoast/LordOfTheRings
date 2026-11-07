module TestImport (
  module X,
  module TestImport,
) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame as X
import LordOfTheRings.Classes.HasQueue as X
import LordOfTheRings.Card as X (Card (..), CardCode, CardDef, EncounterCard, genEncounterCard, genPlayerCard, toCardCode)
import LordOfTheRings.Classes.Entity as X (insertEntity, overAttrs, toAttrs, toId)
import LordOfTheRings.Campaign (CampaignLog)
import LordOfTheRings.Scenario.Types (encounterDeckL, scenarioCampaign)
import LordOfTheRings.Scenario as X (Scenario, escapeFromDolGuldur, journeyAlongTheAnduin, passageThroughMirkwood)
import LordOfTheRings.Decklist as X (DeckList (..), deckList, starterDecks, starterDeck)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Hero as X (Hero, HeroAttrs (..))
import LordOfTheRings.Ally as X (AllyAttrs (..))
import LordOfTheRings.Attachment (AttachmentAttrs (..), newAttachment)

import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.Log.Entry (LogRef (..), LogRefKind)
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
import LordOfTheRings.Matcher as X (getDamage, getEncounterDiscard, getLead, getLocation, getPlayer)
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
  deckList
    ["01012", "01007", "01002"]
    [ ("01013", 3), ("01014", 2), ("01015", 2), ("01016", 3), ("01017", 2), ("01031", 1)
    , ("01045", 2), ("01059", 2), ("01060", 1), ("01061", 2), ("01062", 2), ("01073", 3)
    , ("01026", 2), ("01027", 1), ("01057", 1), ("01069", 2), ("01070", 2), ("01072", 2)
    , ("01020", 2), ("01023", 2), ("01025", 1), ("01046", 2), ("01048", 2), ("01050", 2)
    , ("01051", 2), ("01053", 1), ("01064", 1)
    ]

-- | Push a message and run the queue until it empties or a question is asked.
run :: Message -> GameT ()
run msg = runPaused msg >> turnOverQuests

-- | Push a message and run the queue, leaving a stage waiting to be turned over.
runPaused :: Message -> GameT ()
runPaused msg = push msg >> void mainLoop

-- | Click each stage waiting for its side A to be read, as a player does, until none is.
turnOverQuests :: GameT ()
turnOverQuests = do
  waiting <- getsGame \g -> [pid | (pid, QuestionLabel l _ _ _) <- Map.toList g.question, "$label.turnOverQuest" `isPrefixOf` l]
  for_ (headMay waiting) \pid -> answerQuestion pid 0 >> turnOverQuests

getQuestion :: PlayerId -> GameT (Maybe (Question Message))
getQuestion pid = Map.lookup pid <$> getsGame (.question)

-- | Answer the open question with the first choice that matches.
chooseOptionMatching :: (HasCallStack) => Text -> PlayerId -> (UI Message -> Bool) -> GameT ()
chooseOptionMatching reason pid f = do
  question <- maybe (error $ "no question when trying to " <> unpack reason) pure =<< getQuestion pid
  case findIndex f (questionChoices question) of
    Nothing -> error $ "could not " <> unpack reason
    Just n -> answerQuestion pid n >> turnOverQuests

chooseLabel :: (HasCallStack) => Text -> PlayerId -> GameT ()
chooseLabel l pid = chooseOptionMatching ("choose " <> l) pid \case
  Label l' _ -> l == l'
  _ -> False

chooseTarget :: (HasCallStack, Targetable a) => PlayerId -> a -> GameT ()
chooseTarget pid (toTarget -> target) = chooseOptionMatching "choose target" pid \case
  TargetLabel target' _ -> target == target'
  _ -> False

-- | What the board is told forbids playing the card in hand, by kind: Nothing when free.
publicHandBlockers :: Card -> GameT (Maybe [LogRefKind])
publicHandBlockers card = do
  seen <- getsGame publicGame
  let prohibitions = parseMaybe (withObject "game" (.: "playProhibitions")) seen :: Maybe (Map CardId [LogRef])
  pure $ map (.logRefKind) <$> (Map.lookup card.id =<< prohibitions)

putCardIntoHand :: PlayerId -> CardDef -> GameT Card
putCardIntoHand pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  let updated = g & playersL . ix pid . handL %~ (card :)
  putGame updated
  putGame . gainedAbilities =<< withHandModifiers =<< preloadModifiers updated
  pure card

-- | Play the player's cards as printed, as a deck loaded without the FAQ's errata.
withoutErrata :: PlayerId -> GameT ()
withoutErrata pid = do
  g <- getGame
  putGame $ g & playersL . ix pid . errataL .~ False

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
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)

-- | At a saga's setup, take the Ring-bearer with the code, of those the game offers.
chooseRingBearer :: (HasCallStack) => PlayerId -> CardCode -> GameT ()
chooseRingBearer pid code = chooseOptionMatching ("choose Ring-bearer " <> tshow code) pid \case
  TargetLabel _ msgs -> or [toCardCode card == code | PutHeroIntoPlayAs _ _ card <- msgs]
  _ -> False

-- | Click the active stage to resolve its When Revealed effect.
resolveWhenRevealed :: (HasCallStack) => PlayerId -> GameT ()
resolveWhenRevealed pid = chooseTarget pid =<< getActiveQuest

-- | Click each card waiting for the lead to resolve its When Revealed
-- effect, in turn, until none is left waiting, turning over the stages that wait first.
resolveRevealed :: GameT ()
resolveRevealed = do
  turnOverQuests
  lead <- getLead
  getQuestion lead >>= \case
    Just (QuestionLabel l _ _ _) | "$label.resolveWhenRevealed" `isPrefixOf` l -> do
      chooseOptionMatching "resolve When Revealed" lead (const True)
      resolveRevealed
    _ -> pure ()

-- | Process exactly one message, leaving whatever it queues unprocessed.
runOnly :: Message -> GameT ()
runOnly msg = push msg >> void runNext

getActiveQuest :: (HasCallStack) => GameT Quest
getActiveQuest = getsGame (fromJustNote "no active quest" . headMay . toList . (.entities.quests))

setEncounterDeck :: [CardDef] -> GameT [EncounterCard]
setEncounterDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  let updated = g & scenarioL %~ overAttrs (encounterDeckL .~ cards)
  putGame updated
  putGame . gainedAbilities =<< withHandModifiers =<< preloadModifiers updated
  pure cards


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
coreDeck sphere = fromJustNote "no such starter deck" $ lookup sphere starterDecks

getCharacterDamage :: Target -> GameT Int
getCharacterDamage = getDamage

-- | Decline any open action window (planning or otherwise), for tests about what follows.
passActions :: PlayerId -> GameT ()
passActions pid =
  getQuestion pid >>= \case
    Just (QuestionLabel prompt _ _ q) | prompt `elem` ["$label.takeAction", "$label.planningActions", "$label.noRemainingActions"] ->
      case findIndex (\case Label l _ -> l `elem` ["$label.pass", "$label.donePlanning"] || "$label.proceedTo." `isPrefixOf` l; _ -> False) (questionChoices q) of
        Just n -> answerQuestion pid n >> passActions pid
        Nothing -> pure ()
    _ -> pure ()

-- | Turn over the enemy's shadow cards in the order they were dealt, as the first player
-- is asked when there are several.
resolveShadowsInOrder :: PlayerId -> GameT ()
resolveShadowsInOrder pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.chooseShadowCard" _ _ _) -> answerQuestion pid 0 >> resolveShadowsInOrder pid
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
  -- the modifiers are read from the game with the attachment in it
  g <- (entitiesL . attachmentsL %~ insertEntity att) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
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

-- | Run a test against a one player game of the scenario in campaign mode, with the
-- campaign log as the scenario begins.
campaignTestIn :: Scenario -> CampaignLog -> DeckList -> (PlayerId -> GameT ()) -> IO ()
campaignTestIn scenario l = gameTestIn (overAttrs (\a -> a {scenarioCampaign = Just l}) scenario)

-- | A table of two playing the scenario in campaign mode.
twoPlayerCampaignTestIn :: Scenario -> CampaignLog -> DeckList -> (PlayerId -> PlayerId -> GameT ()) -> IO ()
twoPlayerCampaignTestIn scenario l = twoPlayerTestIn (overAttrs (\a -> a {scenarioCampaign = Just l}) scenario)

-- | The campaign log the scenario holds, as resolved once it is won.
getCampaignLog :: GameT CampaignLog
getCampaignLog = getsGame (fromJustNote "not in campaign mode" . scenarioCampaign . toAttrs . (.scenario))

-- | Put a quest stage straight into play, with nothing else in the quest deck.
putQuestStage :: CardDef -> GameT Target
putQuestStage def = do
  card <- genEncounterCard def
  run $ PutQuestIntoPlay card
  toTarget <$> getActiveQuest

-- | A table of two: the test deck, then the given deck.
twoPlayerTest :: DeckList -> (PlayerId -> PlayerId -> GameT ()) -> IO ()
twoPlayerTest = twoPlayerTestIn passageThroughMirkwood

-- | A table of two in the scenario: the test deck, then the given deck.
twoPlayerTestIn :: Scenario -> DeckList -> (PlayerId -> PlayerId -> GameT ()) -> IO ()
twoPlayerTestIn scenario other body = do
  seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", other)])
  env <- newGameEnv (newTableIn scenario seats) (const $ pure ()) 1
  runGameT env do
    order <- getsGame (.playerOrder)
    case order of
      [one, two] -> body one two
      _ -> error "two players"

heroOf :: PlayerId -> CardCode -> GameT Target
heroOf pid code =
  getsGame (toTarget . fromJustNote "no hero" . find (\h -> toCardCode h == code && h.owner == pid) . toList . (.entities.heroes))
