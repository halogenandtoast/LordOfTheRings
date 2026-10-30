module LordOfTheRings.LostRealm.DeadmensDikeSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getTimeCounters)
import LordOfTheRings.Message.Lifted (cancelWhenRevealed)
import LordOfTheRings.Modifier (ModifierType (CannotHaveAttachments, GainKeyword))
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (deadmensDike, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (CharacterDestroyedByAttack, Explored, PhaseEnds), mkWhen)
import TestImport

dikeTest :: (PlayerId -> GameT ()) -> IO ()
dikeTest = gameTestIn deadmensDike testDeck

codes :: [EncounterCard] -> [CardCode]
codes = map toCardCode

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

questOf :: CardDef -> GameT Target
questOf def = getsGame (fromJustNote "no such quest" . fmap toTarget . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

activeAt :: CardDef -> GameT LocationId
activeAt def = do
  run . PutIntoPlayAsActiveLocation =<< genEncounterCard def
  locationOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The stage in play, turned to its B side, its setup or When Revealed skipped.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  q <- putQuestStage def
  g <- getGame
  putGame $ g & entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})
  pure q

-- | The player's deck, top first, made of these cards.
setDeck :: PlayerId -> [CardDef] -> GameT ()
setDeck pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL .~ cards

-- | The player's discard pile, top first, made of these cards.
setDiscard :: PlayerId -> [CardDef] -> GameT ()
setDiscard pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . discardL .~ cards

deckSize :: PlayerId -> GameT Int
deckSize pid = length . (.deck) <$> getPlayer pid

discardSize :: PlayerId -> GameT Int
discardSize pid = length . (.discard) <$> getPlayer pid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

endOfRefresh :: GameT ()
endOfRefresh = run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]

spec :: Spec
spec = describe "Deadmen's Dike" do
  it "is a scenario the game can be dealt" . dikeTest $ \_ ->
    isJust (lookupScenario "deadmens-dike") `shouldBe` True

  -- \* The stages

  it "sets Thaurdir aside, adds Fornost Square, engages a Baleful Shade per player and reveals a card per player" . dikeTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    _ <- setEncounterDeck [Enemies.thaurdirDeadmensDike, Locations.fornostSquare, Enemies.balefulShade, Enemies.balefulShade, Locations.ancientCauseway]
    chooseOptionMatching "flip the quest" pid (const True)
    getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.thaurdirDeadmensDike]
    placementOf Locations.fornostSquare `shouldReturn` Just StagingArea
    resolveWhenRevealed pid
    placementOf Enemies.balefulShade `shouldReturn` Just (EngagedWith pid)
    -- the other Baleful Shade or the Ancient Causeway is revealed, the other left
    length <$> getEncounterDeck `shouldReturn` 1
    getsGame (length . (.entities.enemies)) >>= \n -> do
      causeway <- isJust <$> placementOf Locations.ancientCauseway
      (n, causeway) `shouldSatisfy` (`elem` [(2, False), (1, True)])

  it "has each player discard the top card of his deck at the end of the refresh phase" . dikeTest $ \pid -> do
    _ <- flippedStage Quests.theShadesOfAngmar
    n <- deckSize pid
    endOfRefresh
    deckSize pid `shouldReturn` n - 1

  it "eliminates a player with no cards in his deck" . dikeTest $ \pid -> do
    _ <- flippedStage Quests.theShadesOfAngmar
    setDeck pid [Allies.guardOfTheCitadel]
    endOfRefresh
    outcome `shouldReturn` Just Lost

  it "does not eliminate a player with an empty deck in another scenario's stage" . dikeTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    setDeck pid [Allies.guardOfTheCitadel]
    run $ DiscardTopOfDeck pid 1
    outcome `shouldReturn` Nothing

  it "adds Thaurdir and reveals a card per player at A Fell Wraith, defeated only once Thaurdir has no hit points left" . dikeTest $ \_ -> do
    thaurdir <- genEncounterCard Enemies.thaurdirDeadmensDike
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [thaurdir])
    q <- putQuestStage Quests.aFellWraith
    _ <- setEncounterDeck [Locations.ancientCauseway]
    run $ Flip q
    lead <- getLead
    resolveWhenRevealed lead
    placementOf Enemies.thaurdirDeadmensDike `shouldReturn` Just StagingArea
    placementOf Locations.ancientCauseway `shouldReturn` Just StagingArea
    run $ PlaceTokens GameSource q Progress 13
    outcome `shouldReturn` Nothing
    eid <- enemyOf Enemies.thaurdirDeadmensDike
    run $ DealDamage GameSource (toTarget eid) 9
    -- Indestructible: he stays, with no hit points left
    placementOf Enemies.thaurdirDeadmensDike `shouldReturn` Just StagingArea
    outcome `shouldReturn` Just Won

  it "has the controller of a character Thaurdir destroys discard 3 cards or return him to the staging area" . dikeTest $ \pid -> do
    _ <- flippedStage Quests.aFellWraith
    staged Enemies.thaurdirDeadmensDike
    eid <- enemyOf Enemies.thaurdirDeadmensDike
    run $ EngageEnemy pid eid
    beravor <- toTarget <$> getHeroWithCode "01012"
    n <- deckSize pid
    run $ CheckWindows [Window After $ CharacterDestroyedByAttack eid beravor pid]
    chooseLabel "$label.aFellWraith.discard" pid
    deckSize pid `shouldReturn` n - 3
    run $ CheckWindows [Window After $ CharacterDestroyedByAttack eid beravor pid]
    chooseLabel "$label.aFellWraith.returnThaurdir" pid
    placementOf Enemies.thaurdirDeadmensDike `shouldReturn` Just StagingArea

  -- \* Enemies

  it "heals Thaurdir and has him attack the first player after a Sorcery treachery is revealed" . dikeTest $ \pid -> do
    staged Enemies.thaurdirDeadmensDike
    eid <- enemyOf Enemies.thaurdirDeadmensDike
    run $ DealDamage GameSource (toTarget eid) 5
    _ <- setEncounterDeck [Treacheries.unnaturalFog, Locations.ancientCauseway]
    run RevealEncounterCard
    resolveRevealed
    getDamage (toTarget eid) `shouldReturn` 2
    getQuestion pid >>= (`shouldSatisfy` isJust)
    getModifiers (toTarget eid) >>= (`shouldSatisfy` elem CannotHaveAttachments)

  it "has Thaurdir's Damned discard 2 cards and deal their combined cost in damage as it engages" . dikeTest $ \pid -> do
    setDeck pid [Allies.snowbournScout, Allies.guardOfTheCitadel, Allies.faramir]
    staged Enemies.thaurdirsDamned
    run . EngageEnemy pid =<< enemyOf Enemies.thaurdirsDamned
    deckSize pid `shouldReturn` 1
    beravor <- toTarget <$> getHeroWithCode "01012"
    replicateM_ 3 $ chooseTarget pid beravor
    getDamage beravor `shouldReturn` 3

  it "has Baleful Shade discard the defending player's top card, +2 attack for an ally" . dikeTest $ \pid -> do
    setDeck pid [Allies.guardOfTheCitadel, Events.everVigilant]
    staged Enemies.balefulShade
    eid <- enemyOf Enemies.balefulShade
    run $ EngageEnemy pid eid
    run $ EnemyAttack pid eid
    deckSize pid `shouldReturn` 1
    getEnemyAttack eid `shouldReturn` 4

  it "has Dead Lord bring the topmost Undead enemy in the discard pile in engaged" . dikeTest $ \pid -> do
    damned <- genEncounterCard Enemies.cursedDead
    causeway <- genEncounterCard Locations.ancientCauseway
    run $ AddToEncounterDiscard [causeway, damned]
    staged Enemies.deadLord
    run . EngageEnemy pid =<< enemyOf Enemies.deadLord
    placementOf Enemies.cursedDead `shouldReturn` Just (EngagedWith pid)
    codes <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.ancientCauseway]

  it "puts each copy of Cursed Dead in the discard pile into play with Cursed Dead" . dikeTest $ \_ -> do
    copies <- replicateM 2 $ genEncounterCard Enemies.cursedDead
    run $ AddToEncounterDiscard copies
    reveal [Enemies.cursedDead]
    getsGame (length . (.entities.enemies)) `shouldReturn` 3
    getEncounterDiscard `shouldReturn` []

  -- \* Locations

  it "discards each player's top 5 and resets as Broken Battlements' last time counter goes, and discards 1 to travel" . dikeTest $ \pid -> do
    staged Locations.brokenBattlements
    lid <- locationOf Locations.brokenBattlements
    n <- deckSize pid
    run $ RemoveTokens GameSource (toTarget lid) TimeCounter 3
    deckSize pid `shouldReturn` n - 5
    getTimeCounters (toTarget lid) `shouldReturn` 1
    run $ TravelTo lid
    deckSize pid `shouldReturn` n - 6

  it "returns the topmost enemy in the encounter discard pile as the players travel to Norbury Tombs, and shuffles 5 back once explored" . dikeTest $ \pid -> do
    shade <- genEncounterCard Enemies.balefulShade
    causeway <- genEncounterCard Locations.ancientCauseway
    run $ AddToEncounterDiscard [shade, causeway]
    staged Locations.norburyTombs
    lid <- locationOf Locations.norburyTombs
    run $ TravelTo lid
    placementOf Enemies.balefulShade `shouldReturn` Just StagingArea
    setDiscard pid [Allies.guardOfTheCitadel, Allies.faramir, Allies.snowbournScout, Allies.guardOfTheCitadel, Allies.faramir, Allies.snowbournScout]
    n <- deckSize pid
    run $ PlaceLocation lid ActiveLocation
    run $ CheckWindows [mkWhen $ Explored lid]
    chooseAbilityOf pid lid
    deckSize pid `shouldReturn` n + 5
    discardSize pid `shouldReturn` 1

  it "puts a resource on Fornost Square as the first player discards from his deck, +1 threat each" . dikeTest $ \pid -> do
    staged Locations.fornostSquare
    lid <- locationOf Locations.fornostSquare
    run $ DiscardTopOfDeck pid 2
    getLocationThreat lid `shouldReturn` 2

  it "gives the first Undead enemy revealed each round surge while Haunted Keep is active" . dikeTest $ \_ -> do
    _ <- activeAt Locations.hauntedKeep
    reveal [Enemies.cursedDead, Locations.ancientCauseway]
    placementOf Locations.ancientCauseway `shouldReturn` Just StagingArea
    reveal [Enemies.deadLord, Locations.ruinsOfArnor]
    placementOf Locations.ruinsOfArnor `shouldReturn` Nothing
    eid <- enemyOf Enemies.cursedDead
    getModifiers (toTarget eid) >>= (`shouldSatisfy` elem (GainKeyword Surge))

  it "has a player playing an event discard his top card while Deadmen's Gate is active" . dikeTest $ \pid -> do
    _ <- activeAt Locations.deadmensGate
    n <- deckSize pid
    card <- genPlayerCard Events.everVigilant (Just pid)
    run $ PlayEvent pid card []
    deckSize pid `shouldReturn` n - 1

  it "raises each player's threat by 2 as a treachery's When Revealed is canceled with Ruins of Arnor in play" . dikeTest $ \pid -> do
    staged Locations.ruinsOfArnor
    threat <- threatOf pid
    _ <- setEncounterDeck [Treacheries.tragicDiscovery]
    -- the treachery is in play, its When Revealed waiting on the queue
    runOnly RevealEncounterCard
    void runNext
    tid <- getsGame (fromJustNote "no treachery" . fmap toTarget . headMay . toList . (.entities.treacheries))
    runQueueT $ cancelWhenRevealed tid
    void mainLoop
    threatOf pid `shouldReturn` threat + 2

  it "raises each player's threat by 2 as Ancient Causeway is explored" . dikeTest $ \pid -> do
    lid <- activeAt Locations.ancientCauseway
    threat <- threatOf pid
    run $ CheckWindows [mkWhen $ Explored lid]
    threatOf pid `shouldReturn` threat + 2

  -- \* Treacheries

  it "makes each player discard for his questing characters or remove them from the quest with Unnatural Fog" . dikeTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ CommitToQuest beravor
    run $ CommitToQuest eowyn
    n <- deckSize pid
    reveal [Treacheries.unnaturalFog]
    chooseLabel "$label.unnaturalFog.discard count=i:2" pid
    deckSize pid `shouldReturn` n - 2
    reveal [Treacheries.unnaturalFog]
    chooseLabel "$label.unnaturalFog.removeFromQuest" pid
    deckSize pid `shouldReturn` n - 2

  it "raises each player's threat by the active location's threat with Tragic Discovery" . dikeTest $ \pid -> do
    _ <- activeAt Locations.ancientCauseway
    threat <- threatOf pid
    reveal [Treacheries.tragicDiscovery]
    threatOf pid `shouldReturn` threat + 3

  it "strengthens each Undead enemy with Restless Evil, surging when there is none" . dikeTest $ \_ -> do
    reveal [Treacheries.restlessEvil, Locations.ancientCauseway]
    placementOf Locations.ancientCauseway `shouldReturn` Just StagingArea
    staged Enemies.cursedDead
    eid <- enemyOf Enemies.cursedDead
    reveal [Treacheries.restlessEvil]
    getEnemyAttack eid `shouldReturn` 4
    getEnemyDefense eid `shouldReturn` 2

  it "adds 2 staging threat for each card type the players discard with Terror of the North" . dikeTest $ \pid -> do
    setDeck pid [Allies.guardOfTheCitadel, Events.everVigilant, Allies.faramir, Allies.snowbournScout]
    threat <- getStagingThreat
    reveal [Treacheries.terrorOfTheNorth]
    deckSize pid `shouldReturn` 1
    getStagingThreat `shouldReturn` threat + 4

  it "discards each ally with a copy in its owner's discard pile with Dark Sorcery" . dikeTest $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    scout <- putAllyIntoPlay pid Allies.snowbournScout
    setDiscard pid [Allies.guardOfTheCitadel]
    reveal [Treacheries.darkSorcery]
    allies <- getsGame (map toTarget . toList . (.entities.allies))
    allies `shouldBe` [scout]

  it "raises the cost of a card with a copy in its owner's discard pile once Heavy Curse is attached to the quest" . dikeTest $ \pid -> do
    _ <- putQuestStage Quests.theShadesOfAngmar
    card <- putCardIntoHand pid Allies.guardOfTheCitadel
    other <- putCardIntoHand pid Allies.faramir
    setDiscard pid [Allies.guardOfTheCitadel]
    reveal [Treacheries.heavyCurse, Locations.ancientCauseway]
    costModifier pid card `shouldReturn` 1
    costModifier pid other `shouldReturn` 0

  -- \* Side quests

  it "discards a card for each Undead enemy engaged with a player at the end of the refresh phase, with The Shadow World" . dikeTest $ \pid -> do
    reveal [Quests.theShadowWorld, Enemies.cursedDead]
    run . EngageEnemy pid =<< enemyOf Enemies.cursedDead
    n <- deckSize pid
    endOfRefresh
    deckSize pid `shouldReturn` n - 1

  it "discards 3 encounter cards and returns the topmost Undead enemy at the end of the refresh phase, with Seal the Tomb" . dikeTest $ \_ -> do
    reveal [Quests.sealTheTomb, Locations.ancientCauseway]
    _ <- setEncounterDeck [Locations.ruinsOfArnor, Enemies.deadLord, Locations.ruinsOfArnor, Locations.fornostSquare]
    endOfRefresh
    placementOf Enemies.deadLord `shouldReturn` Just StagingArea
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Locations.fornostSquare]

  it "keeps the cards in each discard pile from player card effects, then shuffles 5 back once The Power of Angmar is defeated" . dikeTest $ \pid -> do
    reveal [Quests.thePowerOfAngmar, Locations.ancientCauseway]
    setDiscard pid [Allies.guardOfTheCitadel, Allies.faramir, Allies.snowbournScout, Allies.guardOfTheCitadel, Allies.faramir, Allies.snowbournScout]
    top <- getsGame (fromJustNote "no discard" . fmap (.id) . headMay . (.discard) . fromJustNote "no player" . headMay . toList . (.players))
    run $ ReturnFromDiscardToHand pid top
    discardSize pid `shouldReturn` 6
    n <- deckSize pid
    q <- questOf Quests.thePowerOfAngmar
    run $ SetCurrentQuest q
    run $ PlaceProgressOnQuest 8
    deckSize pid `shouldReturn` n + 5
    discardSize pid `shouldReturn` 1
