module LordOfTheRings.VengeanceOfMordor.UnderTheAshMountainsSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (printedCostOf)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, underTheAshMountains)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

ashTest :: (PlayerId -> GameT ()) -> IO ()
ashTest = gameTestIn underTheAshMountains testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyPlacements :: GameT [(CardCode, Placement)]
enemyPlacements = getsGame (map (\e -> (toCardCode e, e.placement)) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

questCodes :: GameT [CardCode]
questCodes = getsGame (map toCardCode . toList . (.entities.quests))

questOf :: CardDef -> GameT Target
questOf def = getsGame (toTarget . fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

-- | A stage put into play on its B side.
stageB :: CardDef -> GameT Target
stageB def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

sideQuest :: CardDef -> GameT Target
sideQuest def = do
  run . RevealCard =<< genEncounterCard def
  resolveRevealed
  questOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

deckSize :: PlayerId -> GameT Int
deckSize pid = length . (.deck) <$> getPlayer pid

discardPile :: PlayerId -> GameT [Card]
discardPile pid = (.discard) <$> getPlayer pid

-- | The deck run down to the given number of cards.
deckDownTo :: PlayerId -> Int -> GameT ()
deckDownTo pid n = do
  k <- deckSize pid
  run $ DiscardTopOfDeck pid (k - n)

-- | A card of the player's put on top of his deck.
onTopOfDeck :: PlayerId -> CardDef -> GameT Card
onTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  run $ PutOnTopOfDeck pid card
  pure card

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

roundEnds :: PlayerId -> GameT ()
roundEnds pid = do
  run $ CheckWindows [mkWhen RoundEnds]
  passActions pid

{- | The enemy attacks the player, the shadow card dealt to it first, and the defender
declares against it.
-}
shadowAttack :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
shadowAttack pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

chooseAnyTarget :: HasCallStack => PlayerId -> GameT ()
chooseAnyTarget pid = chooseOptionMatching "choose a target" pid \case
  TargetLabel {} -> True
  _ -> False

setOf :: EncounterCard -> Maybe EncounterSet
setOf = cdEncounterSet . toCardDef

spec :: Spec
spec = describe "Under the Ash Mountains" do
  it "is a scenario the game can be dealt" . ashTest $ \_ ->
    isJust (lookupScenario "under-the-ash-mountains") `shouldBe` True

  -- \* Setup

  it "builds the encounter deck from Under the Ash Mountains and Twisted Tunnels, 38 cards" . ashTest $ \_ -> do
    run $ DoStep 1 Setup
    deck <- (.encounterDeck) <$> scenarioAttrs
    length deck `shouldBe` 38
    all ((`elem` [Just UnderTheAshMountains, Just TwistedTunnels]) . setOf) deck `shouldBe` True
    map toCardCode . (.questDeck) <$> scenarioAttrs `shouldReturn` [toCardCode Quests.anEvilPlace]

  it "builds a 29 card encounter deck in easy mode" . gameTestIn (overAttrs (modeL .~ EasyMode) underTheAshMountains) testDeck $ \_ -> do
    run $ DoStep 1 Setup
    length . (.encounterDeck) <$> scenarioAttrs `shouldReturn` 29

  it "trims the deck to 40, stages Torech Gorgor Patrol and reveals the first side quest discarded, shuffling the discard pile back in" . ashTest $ \pid -> do
    flipFirstQuest pid
    resolveRevealed
    player <- getPlayer pid
    -- Lost in the Dark, revealed, takes 10 of them under the threat dial
    length player.deck + length player.underThreatDial `shouldBe` 40
    map fst <$> enemyPlacements `shouldReturn` [toCardCode Enemies.torechGorgorPatrol]
    sides <- filter (/= toCardCode Quests.passageIntoMordor) <$> questCodes
    length sides `shouldBe` 1
    getEncounterDiscard `shouldReturn` []
    length <$> getEncounterDeck `shouldReturn` 36
    passage <- getsGame (fromJustNote "no stage" . find ((== toCardCode Quests.passageIntoMordor) . toCardCode) . toList . (.entities.quests))
    (toAttrs passage).flipped `shouldBe` True

  it "has each other player add a location unlike those staged" . twoPlayerTestIn underTheAshMountains testDeck $ \p1 p2 -> do
    run Setup
    replicateM_ 2 do
      asked <- filterM (fmap isJust . getQuestion) [p1, p2]
      for_ (take 1 asked) $ chooseLabel "$label.keepHand"
    lead <- getLead
    let other = if lead == p1 then p2 else p1
    chooseOptionMatching "flip the quest" lead (const True)
    chooseAnyTarget other
    resolveRevealed
    staged <- getsGame (filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
    length staged `shouldBe` 1
    for_ [p1, p2] \p -> do
      player <- getPlayer p
      length player.deck + length player.underThreatDial `shouldBe` 40

  -- \* Passage into Mordor and An Evil Place

  it "discards X from each deck at the end of the round, X 1 more than the quest cards in play" . ashTest $ \pid -> do
    _ <- stageB Quests.passageIntoMordor
    n <- deckSize pid
    roundEnds pid
    deckSize pid `shouldReturn` n - 2
    _ <- sideQuest Quests.aHauntForBeasts
    roundEnds pid
    deckSize pid `shouldReturn` n - 5
    (.eliminated) <$> getPlayer pid `shouldReturn` False

  it "eliminates a player whose deck runs out at the end of the round, and no sooner" . ashTest $ \pid -> do
    _ <- stageB Quests.passageIntoMordor
    deckDownTo pid 0
    (.eliminated) <$> getPlayer pid `shouldReturn` False
    roundEnds pid
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "keeps event cards from shuffling a discard pile into a deck" . ashTest $ \pid -> do
    _ <- stageB Quests.passageIntoMordor
    getModifiers GameTarget >>= (`shouldSatisfy` elem EventsCannotShuffleDiscardIntoDeck)
    run $ DiscardTopOfDeck pid 3
    n <- deckSize pid
    _ <- playWith pid "01007" 1 Events.willOfTheWest
    deckSize pid `shouldReturn` n
    length <$> discardPile pid `shouldReturn` 3

  it "reveals a chosen side quest and stages an enemy per player as An Evil Place is revealed, and wins once defeated" . ashTest $ \pid -> do
    passage <- stageB Quests.passageIntoMordor
    evil <- genEncounterCard Quests.anEvilPlace
    g0 <- getGame
    putGame $ g0 & scenarioL %~ overAttrs (questDeckL .~ [evil])
    _ <- setEncounterDeck [Locations.fieryChasm, Enemies.namelessBeast, Locations.orcPassage, Quests.aHauntForBeasts]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Quests.overwhelmingDarkness
    run $ PlaceTokens GameSource passage Progress 25
    resolveWhenRevealed pid
    haunt <- getsGame (fromJustNote "no Haunt" . find ((== toCardCode Quests.aHauntForBeasts) . toCardCode) . (.encounterDeck) . toAttrs . (.scenario))
    chooseTarget pid haunt
    questCodes >>= (`shouldSatisfy` elem (toCardCode Quests.aHauntForBeasts))
    enemyPlacements `shouldReturn` [(toCardCode Enemies.namelessBeast, StagingArea)]
    -- the discard pile went back in first, so the side quest left there is in the deck, or
    -- discarded again on the way to the enemy
    cards <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
    any ((== toCardCode Quests.overwhelmingDarkness) . toCardCode) cards `shouldBe` True
    q <- questOf Quests.anEvilPlace
    run $ PlaceTokens GameSource q Progress 25
    getsGame (.outcome) `shouldReturn` Just Won

  -- \* Encounter side quests

  it "makes the players travel with The Tunnels of Torech Gorgor, and takes no progress until it holds 3 resources, one per explored active location" . ashTest $ \_ -> do
    _ <- putQuestStage Quests.passageIntoMordor
    tunnels <- sideQuest Quests.theTunnelsOfTorechGorgor
    lid <- stageLocation Locations.orcPassage
    getModifiers lid >>= (`shouldSatisfy` elem MustBeTraveledTo)
    getModifiers tunnels >>= (`shouldSatisfy` elem (ProgressLimit 0))
    replicateM_ 3 do
      active <- activeLocation Locations.oldForestRoad
      run $ PlaceTokens GameSource (toTarget active) Progress 6
    getModifiers tunnels >>= (`shouldSatisfy` notElem (ProgressLimit 0))
    run $ PlaceTokens GameSource tunnels Progress 3
    questCodes >>= (`shouldSatisfy` notElem (toCardCode Quests.theTunnelsOfTorechGorgor))

  it "lets no enemy take more than 4 damage a round while A Haunt for Beasts is in play, and is no current quest with an enemy staged" . ashTest $ \pid -> do
    _ <- putQuestStage Quests.passageIntoMordor
    haunt <- sideQuest Quests.aHauntForBeasts
    troll <- engaged pid Enemies.mordorCaveTroll
    run $ DealDamage GameSource (toTarget troll) 3
    run $ DealDamage GameSource (toTarget troll) 3
    getDamage (toTarget troll) `shouldReturn` 4
    getModifiers haunt >>= (`shouldSatisfy` notElem CannotBeCurrentQuest)
    _ <- stage Enemies.spawnOfShelob
    getModifiers haunt >>= (`shouldSatisfy` elem CannotBeCurrentQuest)

  it "has a player discard the top card of his deck as he plays a card while Overwhelming Darkness is in play, and shuffles 3 back as it is defeated" . ashTest $ \pid -> do
    _ <- putQuestStage Quests.passageIntoMordor
    darkness <- sideQuest Quests.overwhelmingDarkness
    n <- deckSize pid
    _ <- playWith pid "01007" 1 Events.willOfTheWest
    passActions pid
    -- no stage bars it here; the empty discard pile is shuffled in, then the top card goes
    length <$> discardPile pid `shouldReturn` 1
    deckSize pid `shouldReturn` n - 1
    run $ DiscardTopOfDeck pid 4
    m <- deckSize pid
    run $ PlaceTokens GameSource darkness Progress 6
    deckSize pid `shouldReturn` m + 3

  -- \* Enemies

  it "has Nameless Beast discard the top card as it attacks, +X attack for its cost, and give +X for the top of the discard pile as a shadow" . ashTest $ \pid -> do
    beast <- engaged pid Enemies.namelessBeast
    top <- onTopOfDeck pid Events.aTestOfWill
    beravor <- hero "01012"
    shadowAttack pid beast Locations.oldForestRoad beravor
    map (.id) <$> discardPile pid >>= (`shouldSatisfy` elem top.id)
    -- 3 + 1 against 2 defense
    getDamage beravor `shouldReturn` 2
    spider <- engaged pid Enemies.forestSpider
    run $ PlaceEnemy beast StagingArea
    theodred <- hero "01002"
    topDiscard <- fromJustNote "no discard" . headMay <$> discardPile pid
    shadowAttack pid spider Enemies.namelessBeast theodred
    -- Forest Spider's 2 + 1 as it engaged, + the cost, against Théodred's 1 defense
    getDamage theodred `shouldReturn` (2 + 1 + printedCostOf topDiscard - 1)

  it "has Mordor Cave Troll take no attachments and discard the top card for each point of excess damage" . ashTest $ \pid -> do
    troll <- engaged pid Enemies.mordorCaveTroll
    getModifiers troll >>= (`shouldSatisfy` elem CannotHaveAttachments)
    n <- deckSize pid
    eowyn <- hero "01007"
    shadowAttack pid troll Locations.oldForestRoad eowyn
    -- 6 against Éowyn's 1 defense and 3 hit points: 2 past them
    deckSize pid `shouldReturn` n - 2

  it "has Ash Mountain Werewolf discard 1 card after it attacks, 2 if it destroyed a character, and 2 more by its shadow if the attack destroys one" . ashTest $ \pid -> do
    wolf <- engaged pid Enemies.ashMountainWerewolf
    n <- deckSize pid
    beravor <- hero "01012"
    shadowAttack pid wolf Locations.oldForestRoad beravor
    deckSize pid `shouldReturn` n - 1
    run $ Ready beravor
    run $ PlaceEnemy wolf StagingArea
    troll <- engaged pid Enemies.mordorCaveTroll
    eowyn <- hero "01007"
    m <- deckSize pid
    shadowAttack pid troll Enemies.ashMountainWerewolf eowyn
    -- 2 for the excess past Éowyn, 2 for the shadow
    deckSize pid `shouldReturn` m - 4

  it "has Spawn of Shelob discard a card or take +2 attack as it engages, and exhaust a character as a shadow" . ashTest $ \pid -> do
    spawn <- stage Enemies.spawnOfShelob
    n <- deckSize pid
    run $ EngageEnemy pid spawn
    chooseLabel "$label.spawnOfShelob.discard" pid
    deckSize pid `shouldReturn` n - 1
    run $ PlaceEnemy spawn StagingArea
    run $ EngageEnemy pid spawn
    chooseLabel "$label.spawnOfShelob.attack" pid
    getEnemyAttack spawn `shouldReturn` 4
    spider <- engaged pid Enemies.forestSpider
    run $ PlaceEnemy spawn StagingArea
    beravor <- hero "01012"
    _ <- setEncounterDeck [Enemies.spawnOfShelob]
    run $ DealShadowCard spider
    inPhase CombatPhase
    run $ EnemyAttacks pid
    passActions pid
    chooseTarget pid beravor
    chooseTarget pid =<< hero "01002"
    passActions pid
    exhausted "01002" `shouldReturn` True

  it "gives Torech Gorgor Patrol X threat, caps the main quest at 5 progress a round while it is staged, and has it attack as it engages" . ashTest $ \pid -> do
    q <- putQuestStage Quests.passageIntoMordor
    patrol <- stage Enemies.torechGorgorPatrol
    getEnemyThreat patrol `shouldReturn` 2
    _ <- sideQuest Quests.aHauntForBeasts
    getEnemyThreat patrol `shouldReturn` 3
    run $ PlaceTokens GameSource q Progress 4
    run $ PlaceTokens GameSource q Progress 4
    (.progress) <$> getActiveQuest `shouldReturn` 5
    beravor <- hero "01012"
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ EngageEnemy pid patrol
    passActions pid
    chooseTarget pid beravor
    passActions pid
    -- 5 against Beravor's 2 defense
    getDamage beravor `shouldReturn` 3

  -- \* Locations

  it "gives Fiery Chasm X threat and has the first player discard X as it is explored" . ashTest $ \pid -> do
    _ <- putQuestStage Quests.passageIntoMordor
    chasm <- stageLocation Locations.fieryChasm
    getLocationThreat chasm `shouldReturn` 2
    n <- deckSize pid
    run $ PlaceTokens GameSource (toTarget chasm) Progress 5
    deckSize pid `shouldReturn` n - 2

  it "places the top 3 cards of each deck under Hideous Deeps and shuffles them back once it is explored as the active location" . ashTest $ \pid -> do
    n <- deckSize pid
    reveal [Locations.hideousDeeps]
    deeps <- locationOf Locations.hideousDeeps
    deckSize pid `shouldReturn` n - 3
    length . (.underneath) . toAttrs <$> getLocation deeps `shouldReturn` 3
    run $ PlaceLocation deeps ActiveLocation
    run $ PlaceTokens GameSource (toTarget deeps) Progress 6
    deckSize pid `shouldReturn` n

  it "loses the cards under Hideous Deeps explored in the staging area" . ashTest $ \pid -> do
    n <- deckSize pid
    reveal [Locations.hideousDeeps]
    deeps <- locationOf Locations.hideousDeeps
    run $ PlaceTokens GameSource (toTarget deeps) Progress 6
    deckSize pid `shouldReturn` n - 3

  it "bars canceling encounter effects while Orc Passage is staged, and adds Torech Gorgor Patrol from the discard pile as it is explored" . ashTest $ \pid -> do
    passage <- stageLocation Locations.orcPassage
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)
    _ <- setEncounterDeck [Locations.fieryChasm]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.torechGorgorPatrol
    run $ PlaceTokens GameSource (toTarget passage) Progress 4
    getQuestion pid >>= traverse_ \_ -> chooseAnyTarget pid
    enemyPlacements `shouldReturn` [(toCardCode Enemies.torechGorgorPatrol, StagingArea)]

  -- \* Treacheries

  it "raises the staging threat by the cost of each card Ransacked Supplies discards, and discards attachments as a shadow" . ashTest $ \pid -> do
    top <- onTopOfDeck pid Events.aTestOfWill
    threat0 <- getStagingThreat
    reveal [Treacheries.ransackedSupplies]
    getStagingThreat `shouldReturn` threat0 + printedCostOf top
    beravor <- hero "01012"
    eowyn <- hero "01007"
    _ <- putAttachmentOn pid Attachments.citadelPlate beravor
    _ <- putAttachmentOn pid Attachments.citadelPlate eowyn
    troll <- engaged pid Enemies.mordorCaveTroll
    _ <- setEncounterDeck [Treacheries.ransackedSupplies]
    run $ DealShadowCard troll
    inPhase CombatPhase
    run $ EnemyAttacks pid
    passActions pid
    chooseTarget pid beravor
    getQuestion pid >>= traverse_ \_ -> chooseAnyTarget pid
    passActions pid
    attachments <- getsGame (length . (.entities.attachments))
    attachments `shouldBe` 1

  it "discards a card from each deck for each character with Burning Reek, and its shadow returns the attacker" . ashTest $ \pid -> do
    n <- deckSize pid
    reveal [Treacheries.burningReek]
    deckSize pid `shouldReturn` n - 3
    spider <- engaged pid Enemies.forestSpider
    _ <- setEncounterDeck [Treacheries.burningReek]
    run $ DealShadowCard spider
    inPhase CombatPhase
    run $ EnemyAttacks pid
    passActions pid
    chooseTarget pid =<< hero "01012"
    chooseLabel "$label.burningReek.return" pid
    passActions pid
    enemyPlacements `shouldReturn` [(toCardCode Enemies.forestSpider, StagingArea)]

  it "has each player assign X damage with Writhing Shadows, exhausting each damaged character, and its shadow attack again" . ashTest $ \pid -> do
    _ <- putQuestStage Quests.passageIntoMordor
    beravor <- hero "01012"
    reveal [Treacheries.writhingShadows]
    chooseTarget pid beravor
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2
    exhausted "01012" `shouldReturn` True
    exhausted "01007" `shouldReturn` False
    spider <- engaged pid Enemies.forestSpider
    eowyn <- hero "01007"
    _ <- setEncounterDeck [Treacheries.writhingShadows, Locations.oldForestRoad]
    run $ DealShadowCard spider
    inPhase CombatPhase
    run $ EnemyAttacks pid
    passActions pid
    chooseTarget pid eowyn
    passActions pid
    -- attacked again: Théodred defends or it goes undefended
    isJust <$> getQuestion pid `shouldReturn` True
