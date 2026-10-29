module LordOfTheRings.AgainstTheShadow.TheStewardsFearSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Frontend (FrontendMessage (..))
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Log.Entry
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theStewardsFear)
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (Meta (..), getMeta)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), metaL)
import LordOfTheRings.Trait (Trait (Clue, Plot, Villain))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (EnemyAttacking, RoundEnds), mkAfter)
import TestImport

fearTest :: (PlayerId -> GameT ()) -> IO ()
fearTest = gameTestIn theStewardsFear testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [o.placement | o <- toList g.entities.objectives, toCardCode o == code]

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

objectiveOf :: CardDef -> GameT ObjectiveId
objectiveOf def = getsGame (fromJustNote "no such objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

meta :: GameT Meta
meta = getsGame (getMeta . toAttrs . (.scenario))

-- | The underworld deck, made of these cards.
setUnderworld :: [CardDef] -> GameT [EncounterCard]
setUnderworld defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (metaL .~ toJSON (Meta cards mempty))
  pure cards

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

questResources :: GameT Int
questResources = countTokens Resource . questTokens . toAttrs <$> getActiveQuest

-- | Through setup to Conspiracy's When Revealed resolved.
setupConspiracy :: PlayerId -> GameT ()
setupConspiracy pid = do
  flipFirstQuest pid
  resolveWhenRevealed pid

-- | An undefended attack by the enemy, its damage dealt to the hero.
undefendedAttackOn :: PlayerId -> EnemyId -> Target -> GameT ()
undefendedAttackOn pid eid hero = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid
  chooseTarget pid hero
  passActions pid

completeActiveQuest :: GameT ()
completeActiveQuest = run . CompleteQuest . toId =<< getActiveQuest

spec :: Spec
spec = describe "The Steward's Fear" do
  it "is a scenario the game can be dealt" . fearTest $ \_ ->
    isJust (lookupScenario "the-stewards-fear") `shouldBe` True

  it "builds the underworld deck, sets a Villain, a Plot and Roots of Mindolluin aside, and opens at The Fourth Star" . fearTest $ \pid -> do
    setupConspiracy pid
    placementOf Locations.theFourthStar `shouldReturn` Just ActiveLocation
    star <- locationOf Locations.theFourthStar
    starCard <- (.cardId) . toAttrs <$> getLocation star
    m <- meta
    -- 3 Pickpockets, 3 Zealous Traitors, 2 Lossarnach Bandits, an Umbar Assassin and 3 Clues,
    -- one of them under The Fourth Star for the one player
    length m.underworldDeck `shouldBe` 11
    length <$> Map.lookup starCard m.underneath `shouldBe` Just 1
    setAside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    let defs = map toCardDef setAside
    length [d | d <- defs, Villain `member` cdCardTraits d] `shouldBe` 1
    length [d | d <- defs, Plot `member` cdCardTraits d] `shouldBe` 1
    map toCardCode setAside `shouldSatisfy` elem (toCardCode Locations.rootsOfMindolluin)
    deck <- map toCardDef <$> getEncounterDeck
    [d | d <- deck, any (`member` cdCardTraits d) [Villain, Plot, Clue]] `shouldBe` []
    [d | d <- deck, cdCardType d == EnemyType, toCardCode d /= toCardCode Enemies.underworldDissident] `shouldBe` []

  it "stacks underworld cards under a location entering play, and stages them as it leaves" . fearTest $ \_ -> do
    _ <- setUnderworld [Enemies.zealousTraitor, Objectives.aPrisoner]
    staged Locations.sewers
    sewers <- locationOf Locations.sewers
    map toCardCode . (.underworldDeck) <$> meta `shouldReturn` [toCardCode Objectives.aPrisoner]
    run $ Discard (toTarget sewers)
    placementOf Enemies.zealousTraitor `shouldReturn` Just StagingArea
    Map.null . (.underneath) <$> meta `shouldReturn` True

  it "shows how many underworld cards are under a location, not which, and logs them stacked" $ do
   (player, heroes) <- either (error . show) pure =<< loadPlayer "tester" testDeck
   logged <- newIORef []
   env <- newGameEnv (newGameIn theStewardsFear player heroes) (\case Logged e -> modifyIORef' logged (e :); _ -> pure ()) 1
   runGameT env do
    _ <- setUnderworld [Enemies.zealousTraitor, Objectives.aPrisoner]
    staged Locations.sewers
    sewers <- (.cardId) . toAttrs <$> (getLocation =<< locationOf Locations.sewers)
    seen <- getsGame publicGame
    let field k = \case Object o -> KeyMap.lookup k o; _ -> Nothing
    field "underneath" seen `shouldBe` Just (toJSON (Map.singleton sewers (1 :: Int)))
    (field "scenario" seen >>= field "meta") `shouldBe` Nothing
    said <- concatMap (\e -> [k | LogI18n k _ <- e.logEntryBody]) <$> readIORef logged
    said `shouldSatisfy` elem "log.placedUnderneath"

  it "places the top underworld card under the active location when Sewers is revealed" . fearTest $ \_ -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.cityStreet
    street <- locationOf Locations.cityStreet
    streetCard <- (.cardId) . toAttrs <$> getLocation street
    _ <- setUnderworld [Enemies.pickpocket, Enemies.zealousTraitor]
    reveal [Locations.sewers]
    sewersCard <- (.cardId) . toAttrs <$> (getLocation =<< locationOf Locations.sewers)
    m <- meta
    -- its When Revealed comes before its own Underworld 1 (ALeP 3.073)
    map toCardCode <$> Map.lookup streetCard m.underneath `shouldBe` Just [toCardCode Enemies.pickpocket]
    map toCardCode <$> Map.lookup sewersCard m.underneath `shouldBe` Just [toCardCode Enemies.zealousTraitor]
    m.underworldDeck `shouldBe` []

  it "advances from Conspiracy once a fourth active location is explored" . fearTest $ \_ -> do
    _ <- putQuestStage Quests.conspiracy
    for_ [1 .. 4 :: Int] \n -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.cityStreet
      street <- locationOf Locations.cityStreet
      run $ PlaceTokens GameSource (toTarget street) Progress 2
      when (n < 4) do
        questResources `shouldReturn` n
        getsGame (.outcome) `shouldReturn` Nothing
    -- with no stage behind it, advancing wins
    getsGame (.outcome) `shouldReturn` Just Won

  it "reveals the Plot and makes Roots of Mindolluin active at The Grand Design, then the Villain at The Confrontation" . fearTest $ \pid -> do
    setupConspiracy pid
    completeActiveQuest
    resolveWhenRevealed pid
    placementOf Locations.rootsOfMindolluin `shouldReturn` Just ActiveLocation
    placementOf Locations.theFourthStar `shouldReturn` Just StagingArea
    plots <- getsGame (filter ((Plot `member`) . cdCardTraits . toCardDef) . toList . (.entities.objectives))
    map (.placement) plots `shouldBe` [StagingArea]
    -- Roots of Mindolluin gives the location in the staging area 2 more quest points
    (getLocationQuestPoints =<< locationOf Locations.theFourthStar) `shouldReturn` 7
    deckBefore <- length <$> getEncounterDeck
    underworld <- length . (.underworldDeck) <$> meta
    completeActiveQuest
    resolveWhenRevealed pid
    villains <- getsGame (filter ((Villain `member`) . cdCardTraits . toCardDef) . toList . (.entities.enemies))
    map (.placement) villains `shouldBe` [StagingArea]
    (.underworldDeck) <$> meta `shouldReturn` []
    length <$> getEncounterDeck `shouldReturn` deckBefore + underworld
    quest <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource quest Progress 15
    getsGame (.outcome) `shouldReturn` Nothing
    for_ villains \v -> run $ Destroy GameSource (toTarget v)
    getsGame (.outcome) `shouldReturn` Just Won

  it "claims A Prisoner into the victory display, placing 2 resources on the quest" . fearTest $ \pid -> do
    _ <- putQuestStage Quests.conspiracy
    staged Objectives.aPrisoner
    oid <- objectiveOf Objectives.aPrisoner
    beravor <- getHeroWithCode "01012"
    run $ ActionWindow pid
    chooseAbilityOf pid (ObjectiveTarget oid)
    chooseTarget pid beravor
    victory <- getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))
    victory `shouldSatisfy` elem (toCardCode Objectives.aPrisoner)
    questResources `shouldReturn` 2

  it "trades Secret Map for 3 progress on the active location" . fearTest $ \pid -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.theFourthStar
    staged Objectives.secretMap
    oid <- objectiveOf Objectives.secretMap
    beravor <- getHeroWithCode "01012"
    run $ ActionWindow pid
    chooseAbilityOf pid (ObjectiveTarget oid)
    chooseTarget pid beravor
    placementOf Objectives.secretMap `shouldReturn` Just (AttachedTo $ toTarget beravor)
    run $ ActionWindow pid
    chooseAbilityOf pid (ObjectiveTarget oid)
    star <- locationOf Locations.theFourthStar
    (.progress) <$> getLocation star `shouldReturn` 3

  it "reveals an underworld card with Unwelcome Discovery, discarding a Clue" . fearTest $ \_ -> do
    _ <- setUnderworld [Objectives.aScrapOfHistory, Enemies.pickpocket]
    reveal [Treacheries.unwelcomeDiscovery, Treacheries.unwelcomeDiscovery, Locations.marketSquare]
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Objectives.aScrapOfHistory))
    placementOf Enemies.pickpocket `shouldReturn` Just StagingArea
    placementOf Locations.marketSquare `shouldReturn` Just StagingArea

  it "has A Knife in the Back turn a random ally's attack on a hero, then discard it" . fearTest $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.faramir
    reveal [Treacheries.aKnifeInTheBack, Locations.marketSquare]
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1
    getsGame (Map.null . (.entities.allies)) `shouldReturn` True
    placementOf Locations.marketSquare `shouldReturn` Nothing

  it "gives A Knife in the Back surge when the first player has no ally" . fearTest $ \_ -> do
    reveal [Treacheries.aKnifeInTheBack, Locations.marketSquare]
    placementOf Locations.marketSquare `shouldReturn` Just StagingArea

  it "ends the quest phase without resolving the quest with False Lead" . fearTest $ \pid -> do
    run $ BeginPhase QuestPhase
    chooseLabel "$label.doneCommitting" pid
    _ <- setEncounterDeck [Treacheries.falseLeadTheStewardsFear, Locations.marketSquare]
    threat <- (.threat) <$> getPlayer pid
    resolveRevealed
    placementOf Locations.marketSquare `shouldReturn` Nothing
    (.threat) <$> getPlayer pid `shouldReturn` threat

  it "removes all tokens from the quest when False Lead's attack destroys a character" . fearTest $ \pid -> do
    quest <- putQuestStage Quests.conspiracy
    run $ PlaceTokens GameSource quest Resource 2
    staged Enemies.underworldDissident
    eid <- enemyOf Enemies.underworldDissident
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.falseLeadTheStewardsFear
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ DealDamage GameSource beravor 3
    undefendedAttackOn pid eid beravor
    questResources `shouldReturn` 0

  it "keeps the quest's tokens when False Lead's attack destroys no one" . fearTest $ \pid -> do
    quest <- putQuestStage Quests.conspiracy
    run $ PlaceTokens GameSource quest Resource 2
    staged Enemies.underworldDissident
    eid <- enemyOf Enemies.underworldDissident
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.falseLeadTheStewardsFear
    undefendedAttackOn pid eid . toTarget =<< getHeroWithCode "01012"
    questResources `shouldReturn` 2

  it "loses the game with Unholy Alliance when an enemy attack destroys a hero, but not otherwise" . fearTest $ \pid -> do
    staged Objectives.unholyAlliance
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ DealDamage GameSource eowyn 10
    getsGame (.outcome) `shouldReturn` Nothing
    staged Enemies.underworldDissident
    eid <- enemyOf Enemies.underworldDissident
    run $ EngageEnemy pid eid
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ DealDamage GameSource beravor 3
    undefendedAttackOn pid eid beravor
    getsGame (.outcome) `shouldReturn` Just Lost

  it "does not lose the game with Unholy Alliance for a hero destroyed some other way while an attack resolves" . fearTest $ \pid -> do
    staged Objectives.unholyAlliance
    staged Enemies.underworldDissident
    eid <- enemyOf Enemies.underworldDissident
    run $ EngageEnemy pid eid
    -- the attack's damage is dealt and its After window waits as the hero is destroyed
    push $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    run . (\h -> DealDamage GameSource h 10) . toTarget =<< getHeroWithCode "01007"
    getsGame (.outcome) `shouldReturn` Nothing

  it "reveals an extra card each staging step with Unholy Alliance" . fearTest $ \pid -> do
    staged Objectives.unholyAlliance
    _ <- setEncounterDeck [Locations.marketSquare, Locations.storehouse, Locations.sewers]
    run Staging
    resolveRevealed
    passActions pid
    placementOf Locations.marketSquare `shouldReturn` Just StagingArea
    placementOf Locations.storehouse `shouldReturn` Just StagingArea
    placementOf Locations.sewers `shouldReturn` Nothing

  it "burns more of each deck each round with Up in Flames, and loses the game once a deck is empty" . fearTest $ \pid -> do
    staged Objectives.upInFlames
    deckSize <- length . (.deck) <$> getPlayer pid
    run $ CheckWindows [Window When RoundEnds]
    length . (.deck) <$> getPlayer pid `shouldReturn` deckSize - 1
    getsGame (.outcome) `shouldReturn` Nothing
    g <- getGame
    putGame $ g & playersL . ix pid . deckL %~ take 2
    run $ CheckWindows [Window When RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Lost

  it "loses the game as Up in Flames enters play with a deck already empty" . fearTest $ \pid -> do
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ []
    staged Objectives.upInFlames
    getsGame (.outcome) `shouldReturn` Just Lost

  it "raises each player's threat by 2 at the end of the round with Poisoned Counsels" . fearTest $ \pid -> do
    staged Objectives.poisonedCounsels
    threat <- (.threat) <$> getPlayer pid
    run $ CheckWindows [Window When RoundEnds]
    (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  it "has The Hand of Castamir attack the first player after a treachery is revealed" . fearTest $ \pid -> do
    staged Enemies.theHandOfCastamir
    reveal [Treacheries.falseLeadTheStewardsFear]
    passActions pid
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)

  it "returns Daughter of Berúthiel to the staging area after she attacks, unless the player raises his threat" . fearTest $ \pid -> do
    staged Enemies.daughterOfBeruthiel
    eid <- enemyOf Enemies.daughterOfBeruthiel
    run $ EngageEnemy pid eid
    undefendedAttackOn pid eid . toTarget =<< getHeroWithCode "01012"
    threat <- (.threat) <$> getPlayer pid
    chooseLabel "$label.daughterOfBeruthiel.raiseThreat" pid
    (.threat) <$> getPlayer pid `shouldReturn` threat + 4
    placementOf Enemies.daughterOfBeruthiel `shouldReturn` Just (EngagedWith pid)
    undefendedAttackOn pid eid . toTarget =<< getHeroWithCode "01007"
    chooseLabel "$label.daughterOfBeruthiel.return" pid
    placementOf Enemies.daughterOfBeruthiel `shouldReturn` Just StagingArea

  it "gives Underworld Dissident +1 attack for each Underworld location in play" . fearTest $ \_ -> do
    staged Enemies.underworldDissident
    eid <- enemyOf Enemies.underworldDissident
    getEnemyAttack eid `shouldReturn` 2
    staged Locations.storehouse
    staged Locations.theFourthStar
    getEnemyAttack eid `shouldReturn` 3

  it "exhausts every character as the players travel to Houses of the Dead" . fearTest $ \_ -> do
    staged Locations.housesOfTheDead
    houses <- locationOf Locations.housesOfTheDead
    run $ TravelTo houses
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "has The Hand of Castamir reveal the top encounter card after it attacks, then discard it" . fearTest $ \pid -> do
    staged Enemies.theHandOfCastamir
    eid <- enemyOf Enemies.theHandOfCastamir
    run $ EngageEnemy pid eid
    _ <- setEncounterDeck [Locations.marketSquare]
    undefendedAttackOn pid eid . toTarget =<< getHeroWithCode "01012"
    placementOf Locations.marketSquare `shouldReturn` Nothing
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.marketSquare))

  it "has Telemnar's Bane attack the first player after an event is played" . fearTest $ \pid -> do
    staged Enemies.telemnarsBane
    _ <- playWith pid "01002" 5 Events.grimResolve
    passActions pid
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)

  it "discards the top 3 cards of each deck as Telemnar's Bane attacks" . fearTest $ \pid -> do
    staged Enemies.telemnarsBane
    eid <- enemyOf Enemies.telemnarsBane
    run $ EngageEnemy pid eid
    deckSize <- length . (.deck) <$> getPlayer pid
    run $ EnemyAttack pid eid
    length . (.deck) <$> getPlayer pid `shouldReturn` deckSize - 3

