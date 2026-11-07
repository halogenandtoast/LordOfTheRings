module LordOfTheRings.Standalone.PrancingPonyInvestigationSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardType (EnemyType, LocationType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Game.Public (publicGameFor)
import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (IgnoreInvestigate))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (Blank, CannotTakeDamage, PlacingQuestingProgress))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Window (WindowType (QuestedUnsuccessfully), mkAfter)
import LordOfTheRings.Scenario (murderAtThePrancingPony)
import LordOfTheRings.Scenario.Types (scenarioMeta)
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

meta :: GameT Meta
meta = getsGame (getMeta . toAttrs . (.scenario))

initialize :: GameT ()
initialize = do
  run $ DoStep 1 Setup
  current <- getActiveQuest
  run $ PerformSetup $ toTarget current

setClues :: [EncounterCard] -> GameT ()
setClues cards = do
  stored <- meta
  current <- getGame
  putGame =<< preloadModifiers (current & scenarioL %~ overAttrs (\a -> a {scenarioMeta = toJSON $ stored {investigationDeck = cards}}))

safeClues :: GameT [EncounterCard]
safeClues = filter ((/= toCardCode Treacheries.takenBySurprise) . toCardCode) . (.investigationDeck) <$> meta

acceptLook :: PlayerId -> GameT ()
acceptLook = chooseLabel "$label.continue"

shown :: PlayerId -> GameT [Card]
shown pid = maybe [] searchedCards <$> getQuestion pid

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (toId . fromJustNote "missing location" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageTwo :: GameT Target
stageTwo = do
  initialize
  target <- toTarget <$> getActiveQuest
  _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
  replicateM_ 3 do
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource target Progress 50
  resolveRevealed
  toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.runningOutOfTime
  toTarget <$> getActiveQuest

stageThree :: PlayerId -> GameT ()
stageThree pid = do
  _ <- stageTwo
  [suspect, hideout] <- (.hidden) <$> meta
  run $ MakeAccusation pid (Just $ toCardCode suspect) (Just $ toCardCode hideout)
  resolveRevealed
  toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.apprehendingTheKiller

spec :: Spec
spec = describe "Prancing Pony investigation" do
  it "sets up fifty physical encounters as a hidden pair, ten clues, two locations and thirty-six encounter cards" . gameTestIn murderAtThePrancingPony testDeck $ \_ -> do
    initialize
    stored <- meta
    length stored.hidden `shouldBe` 2
    sort (map (cdCardType . toCardDef) stored.hidden) `shouldBe` sort [EnemyType, LocationType]
    length stored.investigationDeck `shouldBe` 10
    length (filter ((== toCardCode Treacheries.takenBySurprise) . toCardCode) stored.investigationDeck) `shouldBe` 2
    length <$> getEncounterDeck `shouldReturn` 36
    locations <- getsGame (toList . (.entities.locations))
    length locations `shouldBe` 2
    pony <- getLocation =<< locationOf Locations.thePrancingPonyMurder
    (.placement) pony `shouldBe` ActiveLocation
    let allIds = map (.id) (stored.hidden <> stored.investigationDeck) <> map ((.cardId) . toAttrs) locations
    deck <- getEncounterDeck
    length (nub $ allIds <> map (.id) deck) `shouldBe` 50

  it "preserves deck order and physical identities when a card effect looks at the top cards" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    cards <- safeClues
    setClues cards
    run $ LookAtInvestigationDeck pid GameSource 3
    map (.id) <$> shown pid `shouldReturn` map (.id) (take 3 cards)
    acceptLook pid
    (.investigationDeck) <$> meta `shouldReturn` cards
    Map.lookup pid . (.known) <$> meta `shouldReturn` Just (map toCardCode $ take 3 cards)

  it "looks at the bottom card without shuffling or moving it" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    cards <- safeClues
    setClues cards
    run $ LookAtBottomOfInvestigationDeck pid GameSource
    map (.id) <$> shown pid `shouldReturn` map (.id) (take 1 $ reverse cards)
    acceptLook pid
    (.investigationDeck) <$> meta `shouldReturn` cards

  it "caps a look at the remaining deck and handles an empty deck" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    cards <- take 1 <$> safeClues
    setClues cards
    run $ LookAtInvestigationDeck pid GameSource 20
    length <$> shown pid `shouldReturn` 1
    acceptLook pid
    setClues []
    run $ LookAtInvestigationDeck pid GameSource 2
    getQuestion pid `shouldReturn` Nothing
    Map.lookup pid . (.lastLook) <$> meta `shouldReturn` Just []

  it "hides the solution and another player's looked-at cards from public state" . twoPlayerTestIn murderAtThePrancingPony testDeck $ \one two -> do
    initialize
    cards <- safeClues
    setClues cards
    run $ LookAtInvestigationDeck one GameSource 2
    current <- getGame
    let publicView = publicGameFor (== two) current
        parsed = parseMaybe (withObject "game" (.: "question")) publicView :: Maybe (Map PlayerId (Question Message))
    maybe [] (concatMap searchedCards . toList) parsed `shouldBe` []
    for_ (take 2 cards) $ \card ->
      show (encode publicView) `shouldSatisfy` (not . List.isInfixOf (show card.id))
    let privateMeta = parseMaybe (withObject "game" $ \o -> o .: "scenario" >>= withObject "scenario" (.:? "meta")) publicView :: Maybe (Maybe Value)
    privateMeta `shouldBe` Just Nothing
    length <$> shown one `shouldReturn` 2

  it "shuffles the same physical pool for Investigate and shows the shuffled top two cards" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    cards <- safeClues
    setClues cards
    manor <- locationOf Locations.ramshackleManor
    run $ ResolveInvestigate pid manor
    shuffled <- (.investigationDeck) <$> meta
    sort (map (.id) shuffled) `shouldBe` sort (map (.id) cards)
    map (.id) <$> shown pid `shouldReturn` map (.id) (take 2 shuffled)
    acceptLook pid

  it "ignores Investigate when the game says to, while ordinary card-effect looks still work" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    cards <- safeClues
    setClues cards
    manor <- locationOf Locations.ramshackleManor
    run $ CreateEffect $ Effect GameSource GameTarget [IgnoreInvestigate] UntilEndOfRound
    run $ ResolveInvestigate pid manor
    getQuestion pid `shouldReturn` Nothing
    run $ LookAtInvestigationDeck pid GameSource 2
    length <$> shown pid `shouldReturn` 2

  it "does not investigate when a location is explored in staging" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    cards <- safeClues
    setClues cards
    manor <- locationOf Locations.ramshackleManor
    run $ PlaceTokens GameSource (LocationTarget manor) Progress 3
    getQuestion pid `shouldReturn` Nothing
    (.investigationDeck) <$> meta `shouldReturn` cards

  it "resolves Taken by Surprise, engages the discarded enemy without revealing it, and removes the trap afterward" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    trap <- fromJustNote "trap missing" . find ((== toCardCode Treacheries.takenBySurprise) . toCardCode) . (.investigationDeck) <$> meta
    setClues [trap]
    _ <- setEncounterDeck [Enemies.greenwayFootpad, Locations.oldForestRoad]
    run $ LookAtInvestigationDeck pid GameSource 1
    (.removed) <$> meta `shouldReturn` []
    acceptLook pid
    enemies <- getsGame (toList . (.entities.enemies))
    map toCardCode enemies `shouldBe` [toCardCode Enemies.greenwayFootpad]
    map ((.placement) . toAttrs) enemies `shouldBe` [EngagedWith pid]
    (.investigationDeck) <$> meta `shouldReturn` []
    (.removed) <$> meta `shouldReturn` [trap]
    (.threat) <$> getPlayer pid `shouldReturn` 0
    getQuestion pid `shouldReturn` Nothing

  it "resolves both looked-at traps exactly once each" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    traps <- filter ((== toCardCode Treacheries.takenBySurprise) . toCardCode) . (.investigationDeck) <$> meta
    setClues traps
    _ <- setEncounterDeck [Enemies.greenwayFootpad, Enemies.lawlessRuffian, Locations.oldForestRoad]
    run $ LookAtInvestigationDeck pid GameSource 2
    acceptLook pid
    getsGame (length . (.entities.enemies)) `shouldReturn` 2
    length . (.removed) <$> meta `shouldReturn` 2
    (.investigationDeck) <$> meta `shouldReturn` []

  it "removes a trap even when no enemy can be discarded" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    traps <- take 1 . filter ((== toCardCode Treacheries.takenBySurprise) . toCardCode) . (.investigationDeck) <$> meta
    setClues traps
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ LookAtInvestigationDeck pid GameSource 1
    acceptLook pid
    getsGame (null . (.entities.enemies)) `shouldReturn` True
    (.removed) <$> meta `shouldReturn` traps

  it "removes a random clue without looking at it or triggering a trap" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    traps <- take 1 . filter ((== toCardCode Treacheries.takenBySurprise) . toCardCode) . (.investigationDeck) <$> meta
    setClues traps
    run $ ScenarioSpecific "pony.loseClue" Null
    (.removed) <$> meta `shouldReturn` traps
    (.known) <$> meta `shouldReturn` mempty
    getQuestion pid `shouldReturn` Nothing

  for_ [False, True] $ \correct ->
    it ("stages the actual solution and handles the named cards, accusation correct: " <> show correct) . twoPlayerTestIn murderAtThePrancingPony testDeck $ \one two -> do
      initialize
      stored <- meta
      let named = if correct then stored.hidden else take 1 (filter ((== EnemyType) . cdCardType . toCardDef) stored.investigationDeck) <> take 1 (filter ((== LocationType) . cdCardType . toCardDef) stored.investigationDeck)
      [suspect, hideout] <- pure named
      run $ MakeAccusation one (Just $ toCardCode suspect) (Just $ toCardCode hideout)
      (.accusationCorrect) <$> meta `shouldReturn` Just correct
      (.hidden) <$> meta `shouldReturn` []
      getsGame (length . (.entities.enemies)) `shouldReturn` if correct then 1 else 2
      (.threat) <$> getPlayer one `shouldReturn` if correct then 0 else 3
      (.threat) <$> getPlayer two `shouldReturn` if correct then 0 else 3

  it "offers five public suspect names without showing physical hidden cards" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    run $ MakeAccusation pid Nothing Nothing
    question <- fromJustNote "missing accusation question" <$> getQuestion pid
    length (questionChoices question) `shouldBe` 5
    searchedCards question `shouldBe` []

  it "preserves hidden identities, deck order and remembered clues through save/load" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    initialize
    cards <- safeClues
    setClues cards
    run $ LookAtInvestigationDeck pid GameSource 2
    acceptLook pid
    run $ LookAtInvestigationDeck pid GameSource 3
    pendingLook <- getQuestion pid
    beforeSave <- meta
    current <- getGame
    case eitherDecode (encode current) of
      Left err -> liftIO $ expectationFailure err
      Right restored -> putGame =<< preloadModifiers restored
    meta `shouldReturn` beforeSave
    getQuestion pid `shouldReturn` pendingLook
    acceptLook pid
    Map.lookup pid . (.known) <$> meta `shouldReturn` Just (map toCardCode $ take 3 cards)

  it "enforces the four-progress round limit and advances through the native stage-2 arrival" . gameTestIn murderAtThePrancingPony testDeck $ \_ -> do
    initialize
    target <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource target Progress 50
    (.progress) <$> getActiveQuest `shouldReturn` 4
    run $ PlaceTokens GameSource target Progress 50
    (.progress) <$> getActiveQuest `shouldReturn` 4
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource target Progress 50
    (.progress) <$> getActiveQuest `shouldReturn` 8
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource target Progress 50
    resolveRevealed
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.runningOutOfTime
    getsGame (length . filter ((== toCardCode Locations.oldForestRoad) . toCardCode) . toList . (.entities.locations)) `shouldReturn` 1

  it "offers the accusation only for progress actually placed on stage 2 by questing" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    target <- stageTwo
    run $ PlaceTokens GameSource target Progress 1
    getQuestion pid `shouldReturn` Nothing
    run $ CreateEffect $ Effect GameSource GameTarget [PlacingQuestingProgress] UntilEndOfPhase
    run $ PlaceTokens GameSource target Progress 1
    question <- fromJustNote "missing accusation choice" <$> getQuestion pid
    [label | Label label _ <- questionChoices question] `shouldSatisfy` elem "$label.prancingPony.accuse"

  it "uses stage 2's unsuccessful questing Forced effect without revealing the removed clue" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    _ <- stageTwo
    clues <- take 1 <$> safeClues
    setClues clues
    run $ CheckWindows [mkAfter QuestedUnsuccessfully]
    (.removed) <$> meta `shouldReturn` clues
    (.known) <$> meta `shouldReturn` mempty
    getQuestion pid `shouldReturn` Nothing

  it "advances after the accusation, protects the suspect until its hideout leaves, and wins when no suspect remains" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    stageThree pid
    suspect <- getsGame (toId . fromJustNote "missing suspect" . headMay . toList . (.entities.enemies))
    getModifiers suspect >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ DealDamage GameSource (EnemyTarget suspect) 2
    getDamage (EnemyTarget suspect) `shouldReturn` 0
    hideout <- getsGame (toId . fromJustNote "missing hideout" . find (\l -> toCardCode l `elem` map toCardCode hideouts) . toList . (.entities.locations))
    run $ PlaceTokens GameSource (LocationTarget hideout) Progress 5
    getModifiers suspect >>= (`shouldSatisfy` notElem CannotTakeDamage)
    run $ RemoveFromGame $ EnemyTarget suspect
    getsGame (.outcome) `shouldReturn` Just Won

  it "checks the continuous victory condition when a blank stage regains its text" . gameTestIn murderAtThePrancingPony testDeck $ \pid -> do
    stageThree pid
    stage <- toTarget <$> getActiveQuest
    run $ CreateEffect $ Effect GameSource stage [Blank] UntilEndOfRound
    suspect <- getsGame (toId . fromJustNote "missing suspect" . headMay . toList . (.entities.enemies))
    run $ RemoveFromGame $ EnemyTarget suspect
    getsGame (.outcome) `shouldReturn` Nothing
    run $ EndEffects UntilEndOfRound
    getsGame (.outcome) `shouldReturn` Just Won
