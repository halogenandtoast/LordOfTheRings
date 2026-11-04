module LordOfTheRings.AngmarAwakened.TheDreadRealmSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card.Synthetic qualified as Synthetic
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (MustCommitToQuest))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theDreadRealm)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (PhaseEnds, RoundEnds, StagingStepEnds))
import TestImport

dreadTest :: (PlayerId -> GameT ()) -> IO ()
dreadTest = gameTestIn theDreadRealm testDeck

enemiesOf :: CardDef -> GameT [EnemyId]
enemiesOf def = getsGame (map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = fromJustNote "no such enemy" . headMay <$> enemiesOf def

reanimated :: GameT [EnemyId]
reanimated = enemiesOf Synthetic.reanimatedDead

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

treacheriesOf :: CardDef -> GameT [(TreacheryId, Placement)]
treacheriesOf def = getsGame (map (\t -> (toId t, t.placement)) . filter ((== toCardCode def) . toCardCode) . toList . (.entities.treacheries))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

deckOf :: PlayerId -> GameT [Card]
deckOf pid = (.deck) <$> getPlayer pid

discardOf :: PlayerId -> GameT [Card]
discardOf pid = (.discard) <$> getPlayer pid

-- | Put the card on top of the player's deck.
onTop :: PlayerId -> CardDef -> GameT Card
onTop pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (card :)
  pure card

-- | Defend the enemy's attack with the character.
defendWith :: PlayerId -> EnemyId -> Target -> GameT ()
defendWith pid eid defender = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseTarget pid defender
  passActions pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | Set up, The Catacombs of Carn Dûm flipped, revealing Unholy Crypt.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseLabel "$label.keepHand" pid
  _ <- setEncounterDeck [Locations.unholyCrypt]
  chooseOptionMatching "flip the quest" pid (const True)

spec :: Spec
spec = describe "The Dread Realm" do
  it "is a scenario the game can be dealt" . dreadTest $ \_ ->
    isJust (lookupScenario "the-dread-realm") `shouldBe` True

  -- \* The quest

  it "sets Daechanar and Altar of Midwinter aside, reveals a card per player and reanimates each player's top card" . dreadTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    aside `shouldSatisfy` \cs -> all (`elem` cs) [toCardCode Enemies.daechanar, toCardCode Locations.altarOfMidwinter]
    _ <- setEncounterDeck [Locations.unholyCrypt]
    top <- headMay <$> deckOf pid
    size <- length <$> deckOf pid
    chooseOptionMatching "flip the quest" pid (const True)
    _ <- locationOf Locations.unholyCrypt
    length <$> deckOf pid `shouldReturn` size - 1
    [dead] <- reanimated
    e <- getsGame (fromJustNote "dead" . find ((== dead) . toId) . toList . (.entities.enemies))
    Just (toAttrs e).cardId `shouldBe` fmap (.id) top
    (.placement) e `shouldBe` StagingArea
    getEnemyAttack dead `shouldReturn` 2
    getEnemyDefense dead `shouldReturn` 2

  it "sends a Reanimated Dead to its owner's discard pile as it leaves play" . dreadTest $ \pid -> do
    setUp pid
    [dead] <- reanimated
    e <- getsGame (fromJustNote "dead" . find ((== dead) . toId) . toList . (.entities.enemies))
    run $ DealDamage GameSource (EnemyTarget dead) 2
    reanimated `shouldReturn` []
    map (.id) <$> discardOf pid >>= (`shouldSatisfy` elem (toAttrs e).cardId)
    getEncounterDiscard `shouldReturn` []

  it "moves the Sorcery cards on stage 1 to stage 2B, stages Daechanar and makes Altar of Midwinter active" . dreadTest $ \pid -> do
    setUp pid
    reveal [Treacheries.deathAndCalamity]
    stage1 <- toTarget <$> getActiveQuest
    [(_, AttachedTo q)] <- treacheriesOf Treacheries.deathAndCalamity
    q `shouldBe` stage1
    crypt <- locationOf Locations.unholyCrypt
    run $ PlaceLocation crypt ActiveLocation
    run $ PlaceTokens GameSource stage1 Progress 18
    resolveWhenRevealed pid
    stage2 <- getActiveQuest
    toCardCode stage2 `shouldBe` toCardCode Quests.angmarAwakened
    [(_, AttachedTo q')] <- treacheriesOf Treacheries.deathAndCalamity
    q' `shouldBe` toTarget stage2
    _ <- enemyOf Enemies.daechanar
    altar <- locationOf Locations.altarOfMidwinter
    (.placement) <$> getLocation altar `shouldReturn` ActiveLocation
    (.placement) <$> getLocation crypt `shouldReturn` StagingArea

  it "advances from Angmar Awakened as Daechanar is defeated" . dreadTest $ \pid -> do
    _ <- putQuestStage Quests.angmarAwakened
    _ <- setEncounterDeck [Locations.darkHalls]
    stage3 <- genEncounterCard Quests.daechanarsFall
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (questDeckL .~ [stage3])
    daechanar <- staged Enemies.daechanar
    run $ DealDamage GameSource (EnemyTarget daechanar) 12
    resolveWhenRevealed pid
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.daechanarsFall
    -- Daechanar's Fall adds a location from the encounter deck for the one player
    void $ locationOf Locations.darkHalls

  it "places the progress for Daechanar's Fall on the locations in the staging area" . dreadTest $ \pid -> do
    q <- putQuestStage Quests.daechanarsFall
    halls <- stagedLocation Locations.darkHalls
    crypt <- stagedLocation Locations.unholyCrypt
    run $ PlaceTokens GameSource q Progress 3
    chooseTarget pid halls
    chooseTarget pid crypt
    chooseTarget pid halls
    (.progress) <$> getLocation halls `shouldReturn` 2
    (.progress) <$> getLocation crypt `shouldReturn` 1
    (.progress) <$> getActiveQuest `shouldReturn` 0

  it "damages every character at the end of the quest phase while a location is in play, and wins once none is" . dreadTest $ \pid -> do
    q <- putQuestStage Quests.daechanarsFall
    halls <- stagedLocation Locations.darkHalls
    run $ Flip q
    resolveWhenRevealed pid
    run $ CheckWindows [Window When (PhaseEnds QuestPhase)]
    (getCharacterDamage . toTarget =<< getHeroWithCode "01012") `shouldReturn` 1
    run $ PlaceTokens GameSource (toTarget halls) Progress 4
    run $ CheckWindows [Window When (PhaseEnds QuestPhase)]
    outcome `shouldReturn` Just Won

  -- \* Daechanar

  it "gives Daechanar +1 attack per Sorcery card in play and cancels damage by removing one" . dreadTest $ \pid -> do
    setUp pid
    daechanar <- staged Enemies.daechanar
    getEnemyAttack daechanar `shouldReturn` 4
    reveal [Treacheries.deathAndCalamity]
    getEnemyAttack daechanar `shouldReturn` 5
    run $ DealDamage GameSource (EnemyTarget daechanar) 3
    getDamage (EnemyTarget daechanar) `shouldReturn` 0
    treacheriesOf Treacheries.deathAndCalamity `shouldReturn` []
    getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.deathAndCalamity) . map toCardCode)
    getEnemyAttack daechanar `shouldReturn` 4
    run $ DealDamage GameSource (EnemyTarget daechanar) 3
    getDamage (EnemyTarget daechanar) `shouldReturn` 3

  it "keeps progress off Altar of Midwinter while Daechanar is in play" . dreadTest $ \_ -> do
    altar <- stagedLocation Locations.altarOfMidwinter
    run $ PlaceLocation altar ActiveLocation
    _ <- staged Enemies.daechanar
    run $ PlaceProgressOnQuest 3
    (.progress) <$> getLocation altar `shouldReturn` 0

  it "has the first player reanimate his top card at the end of the staging step with Altar of Midwinter" . dreadTest $ \pid -> do
    _ <- stagedLocation Locations.altarOfMidwinter
    top <- onTop pid Allies.guardOfTheCitadel
    run $ CheckWindows [Window When StagingStepEnds]
    chooseLabel "$label.altarOfMidwinter.reanimate" pid
    [dead] <- reanimated
    e <- getsGame (fromJustNote "dead" . find ((== dead) . toId) . toList . (.entities.enemies))
    (toAttrs e).cardId `shouldBe` top.id

  -- \* Encounter cards

  it "has Death and Calamity reanimate a card for each copy in play" . dreadTest $ \pid -> do
    setUp pid
    reveal [Treacheries.deathAndCalamity]
    length <$> reanimated `shouldReturn` 2
    reveal [Treacheries.deathAndCalamity]
    length <$> reanimated `shouldReturn` 4

  it "has Possession destroy the highest cost ally and possess it in the staging area" . dreadTest $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.snowbournScout
    _ <- putAllyIntoPlay pid Allies.faramir
    reveal [Treacheries.possession]
    [dead] <- reanimated
    [(possession, AttachedTo host)] <- treacheriesOf Treacheries.possession
    host `shouldBe` EnemyTarget dead
    run $ Discard (TreacheryTarget possession)
    reanimated `shouldReturn` []
    map toCardCode <$> discardOf pid >>= (`shouldSatisfy` elem (toCardCode Allies.faramir))

  it "attaches A Fell Dread to a hero, who must quest and adds no willpower" . dreadTest $ \pid -> do
    reveal [Treacheries.aFellDread]
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    getModifiers eowyn >>= (`shouldSatisfy` elem MustCommitToQuest)
    -- reduced to 0 wherever it is read, not only toward the quest
    getWillpower eowyn `shouldReturn` 0
    run $ CommitCharacters pid
    Just q <- getQuestion pid
    [() | Label "$label.doneCommitting" _ <- questionChoices q] `shouldBe` []

  it "attaches a defeated Wraith of Carn Dûm to a character, and returns it to the staging area as that character leaves play" . dreadTest $ \pid -> do
    wraith <- staged Enemies.wraithOfCarnDum
    daechanar <- staged Enemies.daechanar
    eowyn <- toTarget <$> getHeroWithCode "01007"
    attack <- getAttack eowyn
    run $ DealDamage GameSource (EnemyTarget wraith) 5
    chooseTarget pid eowyn
    (.placement) <$> getsGame (fromJustNote "wraith" . find ((== wraith) . toId) . toList . (.entities.enemies))
      >>= (`shouldBe` AttachedTo eowyn)
    getAttack eowyn `shouldReturn` attack - 1
    -- as a Sorcery attachment it powers Daechanar
    getEnemyAttack daechanar `shouldReturn` 5
    run $ Discard eowyn
    [back] <- filter (/= wraith) <$> enemiesOf Enemies.wraithOfCarnDum
    (.placement) <$> getsGame (fromJustNote "wraith" . find ((== back) . toId) . toList . (.entities.enemies))
      >>= (`shouldBe` StagingArea)
    getEncounterDiscard `shouldReturn` []

  it "reanimates the character Dwimmerlaik's attack destroys, with a shadow card" . dreadTest $ \pid -> do
    dwimmerlaik <- staged Enemies.dwimmerlaik
    run $ EngageEnemy pid dwimmerlaik
    scout <- putAllyIntoPlay pid Allies.snowbournScout
    _ <- setEncounterDeck [Locations.darkHalls, Locations.unholyCrypt]
    defendWith pid dwimmerlaik scout
    [dead] <- reanimated
    e <- getsGame (fromJustNote "dead" . find ((== dead) . toId) . toList . (.entities.enemies))
    map toCardCode e.shadowCards `shouldSatisfy` (not . null)
    map toCardCode <$> discardOf pid >>= (`shouldSatisfy` notElem (toCardCode Allies.snowbournScout))

  it "resolves the When Revealed of a Sorcery shadow card dealt to Witch of Angmar" . dreadTest $ \pid -> do
    witch <- staged Enemies.witchOfAngmar
    run $ EngageEnemy pid witch
    _ <- setEncounterDeck [Treacheries.aFellDread]
    run $ DealShadowCard witch
    run $ EnemyAttack pid witch
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    resolveRevealed
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    [(_, AttachedTo host)] <- treacheriesOf Treacheries.aFellDread
    host `shouldBe` eowyn

  it "reanimates a random card from hand with Sinister Dungeon as its shadow card once it is explored" . dreadTest $ \pid -> do
    dungeon <- stagedLocation Locations.sinisterDungeon
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    hand <- length . (.hand) <$> getPlayer pid
    run $ PlaceTokens GameSource (toTarget dungeon) Progress 1
    length . (.hand) <$> getPlayer pid `shouldReturn` hand - 1
    [dead] <- reanimated
    e <- getsGame (fromJustNote "dead" . find ((== dead) . toId) . toList . (.entities.enemies))
    map toCardCode e.shadowCards `shouldBe` [toCardCode Locations.sinisterDungeon]
    getEncounterDiscard `shouldReturn` []

  it "reanimates the allies Tombs of Carn Dûm discards at the end of the round" . dreadTest $ \pid -> do
    _ <- stagedLocation Locations.tombsOfCarnDum
    _ <- onTop pid Allies.guardOfTheCitadel
    run $ CheckWindows [Window When RoundEnds]
    length <$> reanimated `shouldReturn` 1

  it "deals a shadow card to each Undead enemy to travel to Tombs of Carn Dûm" . dreadTest $ \_ -> do
    witch <- staged Enemies.witchOfAngmar
    tombs <- stagedLocation Locations.tombsOfCarnDum
    _ <- setEncounterDeck [Locations.darkHalls, Locations.darkHalls]
    run $ TravelTo tombs
    e <- getsGame (fromJustNote "witch" . find ((== witch) . toId) . toList . (.entities.enemies))
    length e.shadowCards `shouldBe` 1

  it "gives each Reanimated Dead +1 attack and defense from Dark Halls, and Unholy Crypt +1 threat for each" . dreadTest $ \pid -> do
    setUp pid
    [dead] <- reanimated
    crypt <- locationOf Locations.unholyCrypt
    getLocationThreat crypt `shouldReturn` 2
    _ <- stagedLocation Locations.darkHalls
    getEnemyAttack dead `shouldReturn` 3
    getEnemyDefense dead `shouldReturn` 3
