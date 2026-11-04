module LordOfTheRings.Dwarrowdelf.FoundationsOfStoneSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (cdCardCode, cdEncounterSet, toCardDef)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.EncounterSet (EncounterSet (FoundationsOfStone))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyHitPoints)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (OnlyFoughtByKeyword))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Matcher (getEnemy, passesUnique)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (foundationsOfStone)
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (stageFours)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioQuestDeck, scenarioSetAside), questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport
import Data.Aeson (Result (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Game.Public (publicGameFor)
import Data.Map.Strict qualified as Map

fosTest :: (PlayerId -> GameT ()) -> IO ()
fosTest = gameTestIn foundationsOfStone testDeck

-- | A table of two in Foundations of Stone, both with the test deck.
fosTwoPlayerTest :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
fosTwoPlayerTest = fosTableTest testDeck testDeck

-- | A table of two in Foundations of Stone, with the decks given.
fosTableTest :: DeckList -> DeckList -> (PlayerId -> PlayerId -> GameT ()) -> IO ()
fosTableTest deckOne deckTwo body = do
  seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", deckOne) :| [("two", deckTwo)])
  env <- newGameEnv (newTableIn foundationsOfStone seats) (const $ pure ()) 1
  runGameT env do
    order <- getsGame (.playerOrder)
    case order of
      [one, two] -> body one two
      _ -> error "two players"

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The player's deck, top first.
setDeck :: PlayerId -> [CardDef] -> GameT [Card]
setDeck pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL .~ cards
  pure cards

questCodes :: GameT [CardCode]
questCodes = getsGame (map toCardCode . toList . (.entities.quests))

-- | The players in each area, in turn order, or none while the players are together.
areas :: GameT [[PlayerId]]
areas = getsGame \g -> maybe [] (map areaPlayers . areasInTurnOrder g) (gameSeparation g)

-- | Each player alone in an area of his own, the one in focus.
separateEach :: PlayerId -> PlayerId -> GameT ()
separateEach one two = run $ SeparatePlayers [[one], [two]]

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

questDeck :: GameT [CardCode]
questDeck = getsGame (map toCardCode . scenarioQuestDeck . toAttrs . (.scenario))

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

spec :: Spec
spec = describe "Foundations of Stone" do
  it "sets the Foundations of Stone set aside and has Cave Torch attached at setup" . fosTest $ \pid -> do
    flipFirstQuest pid
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    torch <- getsGame (fmap (.placement) . find ((== toCardCode Objectives.caveTorch) . toCardCode) . toList . (.entities.objectives))
    torch `shouldBe` Just (AttachedTo beravor)
    setAside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    setAside `shouldSatisfy` notNull
    setAside `shouldSatisfy` all ((== Just FoundationsOfStone) . cdEncounterSet . toCardDef)
    deck <- getEncounterDeck
    deck `shouldSatisfy` all ((/= Just FoundationsOfStone) . cdEncounterSet . toCardDef)
    questDeck `shouldReturn` map cdCardCode [Quests.theWatersEdge, Quests.washedAway, Quests.outOfTheDepths]

  it "sends Giant Cave Centipede to a different player's own staging area while the players are separated" . fosTwoPlayerTest $ \one two -> do
    separateEach one two
    run . PutEnemyIntoPlayEngaged one =<< genEncounterCard Enemies.giantCaveCentipede
    eid <- enemyOf Enemies.giantCaveCentipede
    run $ UseCardAbility one (EnemySource eid) 1 []
    enemyCodes `shouldReturn` []
    held <- getsGame \g -> [e.placement | Just s <- [gameSeparation g], a <- separationAreas s, two `elem` areaPlayers a, e <- toList (areaHeldEntities a).enemies]
    held `shouldBe` [StagingArea]

  it "has a Nameless Thing take the top 2 cards of the engaged player's deck, its X their printed cost" . fosTest $ \pid -> do
    reveal [Enemies.namelessThing]
    eid <- enemyOf Enemies.namelessThing
    getEnemyAttack eid `shouldReturn` 3
    getEnemyHitPoints eid `shouldReturn` 3
    _ <- setDeck pid [Allies.guardOfTheCitadel, Allies.gondorianSpearman, Allies.snowbournScout]
    run $ EngageEnemy pid eid
    getEnemyAttack eid `shouldReturn` 4
    getEnemyHitPoints eid `shouldReturn` 4
    map toCardCode . (.deck) <$> getPlayer pid `shouldReturn` [toCardCode Allies.snowbournScout]
    -- destroyed, the cards under it go to their owner's discard pile
    run $ Destroy GameSource (EnemyTarget eid)
    map toCardCode . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` ((== 2) . length))

  it "has an Elder Nameless Thing take 3 cards, and be 4 with none" . fosTest $ \pid -> do
    reveal [Enemies.elderNamelessThing]
    eid <- enemyOf Enemies.elderNamelessThing
    getEnemyAttack eid `shouldReturn` 4
    _ <- setDeck pid [Allies.guardOfTheCitadel, Allies.guardOfTheCitadel, Allies.gondorianSpearman]
    run $ EngageEnemy pid eid
    getEnemyAttack eid `shouldReturn` 6
    getEnemyHitPoints eid `shouldReturn` 6

  it "has Deep Deep Dark attach a card from the first player's deck to each Nameless enemy, and surge" . fosTest $ \pid -> do
    reveal [Enemies.namelessThing]
    eid <- enemyOf Enemies.namelessThing
    _ <- setDeck pid [Allies.snowbournScout, Allies.guardOfTheCitadel]
    reveal [Treacheries.deepDeepDark, Enemies.moriaBats]
    getEnemyAttack eid `shouldReturn` 1
    map toCardCode . (.deck) <$> getPlayer pid `shouldReturn` [toCardCode Allies.guardOfTheCitadel]
    void $ enemyOf Enemies.moriaBats

  it "has Lost and Alone shuffle a hero into its owner's deck, put into play as it is drawn" . fosTest $ \pid -> do
    eowyn <- getHeroWithCode "01007"
    reveal [Treacheries.lostAndAlone]
    chooseTarget pid eowyn
    getsGame (any ((== "01007") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` False
    deck <- (.deck) <$> getPlayer pid
    deck `shouldSatisfy` any ((== "01007") . toCardCode)
    run $ DrawCards (Drawing pid (length deck))
    getsGame (any ((== "01007") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` True
    (.hand) <$> getPlayer pid >>= (`shouldSatisfy` all ((/= "01007") . toCardCode))

  it "does not eliminate a player whose last hero Lost and Alone shuffles into his deck" . gameTestIn foundationsOfStone testDeck {heroes = ["01012"]} $ \pid -> do
    reveal [Treacheries.lostAndAlone]
    run CheckEliminations
    (.eliminated) <$> getPlayer pid `shouldReturn` False

  it "lets only ranged characters fight Moria Bats, stronger for each other enemy engaged" . fosTest $ \pid -> do
    reveal [Enemies.moriaBats]
    bats <- enemyOf Enemies.moriaBats
    getModifiers (toTarget bats) >>= (`shouldSatisfy` elem (OnlyFoughtByKeyword Ranged))
    run $ EngageEnemy pid bats
    getEnemyAttack bats `shouldReturn` 1
    reveal [Enemies.namelessThing]
    run . EngageEnemy pid =<< enemyOf Enemies.namelessThing
    getEnemyAttack bats `shouldReturn` 2

  it "surges Durin's Axe, claimed by exhausting a hero for +3 attack" . fosTest $ \pid -> do
    reveal [Objectives.durinsAxe, Enemies.moriaBats]
    void $ enemyOf Enemies.moriaBats
    axe <- getsGame (fromJustNote "no axe" . fmap toTarget . find ((== toCardCode Objectives.durinsAxe) . toCardCode) . toList . (.entities.objectives))
    beravor <- toTarget <$> getHeroWithCode "01012"
    attack <- getAttack beravor
    run $ ActionWindow pid
    chooseAbilityOf pid axe
    chooseTarget pid beravor
    getAttack beravor `shouldReturn` attack + 3

  it "has The Water's Edge discard the top 2 cards of a player's deck once he commits" . fosTest $ \pid -> do
    _ <- putQuestStage Quests.theWatersEdge
    _ <- setDeck pid [Allies.snowbournScout, Allies.guardOfTheCitadel, Allies.gondorianSpearman]
    run $ CommitCharacters pid
    chooseTarget pid =<< getHeroWithCode "01007"
    chooseLabel "$label.doneCommitting" pid
    map toCardCode . (.deck) <$> getPlayer pid `shouldReturn` [toCardCode Allies.gondorianSpearman]

  it "washes away the encounter cards in play, rebuilding the deck and drawing a stage 4" . fosTest $ \pid -> do
    quest <- putQuestStage Quests.washedAway
    setQuestDeck [Quests.outOfTheDepths]
    beravor <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.bladeOfGondolin beravor
    run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.caveTorch
    run . AddToStaging =<< genEncounterCard Enemies.moriaBats
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.branchingPaths
    run . AddToEncounterDiscard . pure =<< genEncounterCard Treacheries.evilStorm
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.dreadfulGap
    _ <- setEncounterDeck [Enemies.goblinFollower]
    run . SetAside =<< genEncounterCard Enemies.namelessThing
    run $ Flip quest
    resolveWhenRevealed pid
    resolveRevealed
    -- the stage drawn is in play, stage 5 behind it
    codes <- questCodes
    codes `shouldSatisfy` all (`elem` map cdCardCode stageFours)
    length codes `shouldBe` 1
    questDeck `shouldReturn` [toCardCode Quests.outOfTheDepths]
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    getsGame (length . (.entities.objectives)) `shouldReturn` 0
    -- the new deck: the discarded enemy, the treachery in the discard pile and the set aside
    deck <- getEncounterDeck
    discard <- getEncounterDiscard
    enemies <- getsGame (map toCardCode . toList . (.entities.enemies))
    locations <- getsGame (map toCardCode . toList . (.entities.locations))
    sort (map toCardCode (deck <> discard) <> enemies <> locations)
      `shouldBe` sort (map cdCardCode [Enemies.moriaBats, Treacheries.evilStorm, Enemies.namelessThing])

  it "gives each player a staging area and a stage 4 of his own, as the only player there" . fosTwoPlayerTest $ \one two -> do
    quest <- putQuestStage Quests.washedAway
    setQuestDeck [Quests.outOfTheDepths]
    _ <- setEncounterDeck []
    run $ Flip quest
    resolveWhenRevealed one
    areas `shouldReturn` [[one], [two]]
    questDeck `shouldReturn` [toCardCode Quests.outOfTheDepths]
    first' <- questCodes
    first' `shouldSatisfy` all (`elem` map cdCardCode stageFours)
    length first' `shouldBe` 1
    getsGame (Map.keys . (.players)) `shouldReturn` [one]
    getLead `shouldReturn` one
    -- a card revealed goes to the staging area of the area in focus alone
    reveal [Enemies.moriaBats]
    enemyCodes `shouldReturn` [toCardCode Enemies.moriaBats]
    run $ FocusArea two
    enemyCodes `shouldReturn` []
    getLead `shouldReturn` two
    second' <- questCodes
    length second' `shouldBe` 1
    second' `shouldSatisfy` (/= first')

  it "shows every area to the players, and keeps the areas when the game is saved" . fosTwoPlayerTest $ \one two -> do
    separateEach one two
    seen <- getsGame (publicGameFor (== one))
    let key k = case seen of
          Object o -> KeyMap.lookup k o
          _ -> Nothing
        players = case key "players" of
          Just (Object ps) -> ps
          _ -> mempty
    sort (map Key.toText $ KeyMap.keys players) `shouldBe` sort (map tshow [one, two])
    (\case Just (Array as) -> length as; _ -> 0) (key "areas") `shouldBe` 2
    key "separation" `shouldBe` Nothing
    saved <- getsGame toJSON
    case fromJSON saved of
      Success (g :: Game) -> maybe [] (map areaPlayers . separationAreas) (gameSeparation g) `shouldBe` [[one], [two]]
      Error e -> error e

  it "keeps the uniqueness rule across the separated areas (FAQ 2.36)" . fosTwoPlayerTest $ \one two -> do
    separateEach one two
    run $ FocusArea two
    _ <- putAllyIntoPlay two Allies.gandalf
    run $ FocusArea one
    gandalf <- PlayerCard <$> genPlayerCard Allies.gandalf (Just one)
    passesUnique gandalf `shouldReturn` False

  it "has Elladan and Elrohir lose their bonuses while separated (ALeP 3.040)" . fosTableTest (testDeck {heroes = ["04001"]}) (testDeck {heroes = ["04028"]}) $ \one two -> do
    elrohir <- heroOf one "04001"
    run $ Run []
    getDefense elrohir `shouldReturn` 3
    separateEach one two
    getDefense elrohir `shouldReturn` 1
    -- together again, the bonus returns
    run $ JoinArea two one
    areas `shouldReturn` []
    getDefense elrohir `shouldReturn` 3

  it "makes each separated player the first player of his area, the token still passing" . fosTwoPlayerTest $ \one two -> do
    separateEach one two
    run $ FocusArea two
    getLead `shouldReturn` two
    -- each area ends the refresh phase and the round in turn, then the token passes
    run $ BeginPhase RefreshPhase
    passActions two
    run $ EndPhase RefreshPhase
    getsGame (fmap separationFirstPlayer . gameSeparation) `shouldReturn` Just two
    getsGame (Map.keys . (.players)) `shouldReturn` [two]
    getLead `shouldReturn` two

  it "has a player whose stage 4 is defeated join another at the beginning of the travel phase" . fosTwoPlayerTest $ \one two -> do
    separateEach one two
    _ <- putQuestStage Quests.oldOneLair
    run $ FocusArea two
    _ <- putQuestStage Quests.theShiveringBank
    reveal [Enemies.moriaBats]
    reveal [Enemies.namelessThing]
    run . EngageEnemy two =<< enemyOf Enemies.namelessThing
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.branchingPaths
    current <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource current Progress 7
    -- the stage stays until its players join another
    questCodes `shouldReturn` [toCardCode Quests.theShiveringBank]
    run JoinAreas
    areas `shouldReturn` []
    questCodes `shouldReturn` [toCardCode Quests.oldOneLair]
    sort <$> enemyCodes `shouldReturn` sort [toCardCode Enemies.moriaBats, toCardCode Enemies.namelessThing]
    nameless <- enemyOf Enemies.namelessThing
    (.placement) <$> getEnemy nameless `shouldReturn` EngagedWith two
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.branchingPaths]

  it "has the players continue on to stage 5 together once no one is left to join" . fosTwoPlayerTest $ \one two -> do
    setQuestDeck [Quests.outOfTheDepths]
    _ <- setEncounterDeck []
    separateEach one two
    oneStage <- putQuestStage Quests.oldOneLair
    run $ PlaceTokens GameSource oneStage Progress 5
    run $ FocusArea two
    twoStage <- putQuestStage Quests.theShiveringBank
    run $ PlaceTokens GameSource twoStage Progress 7
    run JoinAreas
    areas `shouldReturn` []
    questCodes `shouldReturn` [toCardCode Quests.outOfTheDepths]

  it "lets no player commit more allies than heroes to Out of the Depths" . fosTest $ \pid -> do
    _ <- putQuestStage Quests.outOfTheDepths
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    let offersAlly = maybe False (any \case TargetLabel t _ -> t == guard'; _ -> False) . fmap questionChoices
    run $ CommitCharacters pid
    offersAlly <$> getQuestion pid `shouldReturn` False
    chooseTarget pid =<< getHeroWithCode "01007"
    offersAlly <$> getQuestion pid `shouldReturn` True

  it "gives Mithril Lode a Refresh Action placing the exhausted character's willpower on the quest" . fosTest $ \pid -> do
    _ <- putQuestStage Quests.theWatersEdge
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.mithrilLode
    lode <- getsGame (fromJustNote "no lode" . fmap toTarget . headMay . toList . (.entities.locations))
    run $ BeginPhase RefreshPhase
    chooseAbilityOf pid lode
    eowyn <- getHeroWithCode "01007"
    chooseTarget pid eowyn
    getsGame (fmap (.progress) . headMay . toList . (.entities.quests)) `shouldReturn` Just 4
