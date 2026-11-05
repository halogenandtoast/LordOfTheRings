module LordOfTheRings.OnTheDoorstep.FliesAndSpidersNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (..), attachedWithTrait)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Scenario (fliesAndSpiders)
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (..), modeL)
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport hiding (before)

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) fliesAndSpiders

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "no enemy"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.enemies)
    )

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

locationOf :: CardDef -> GameT LocationId
locationOf def =
  getsGame
    ( toId
        . fromJustNote "no location"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.locations)
    )

reveal :: CardDef -> GameT ()
reveal def = do
  _ <- setEncounterDeck [def]
  run RevealEncounterCard
  resolveRevealed

passTableActions :: GameT ()
passTableActions = do
  questions <- getsGame (mapToList . (.question))
  case [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.takeAction"] of
    pid : _ -> passActions pid >> passTableActions
    [] -> pure ()

spec :: Spec
spec = describe "Flies and Spiders Nightmare" do
  it "builds the 42-card deck with the Glade set aside and the exact nightmare removals" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    cards <- getEncounterDeck
    length cards `shouldBe` 42
    let copies def = length $ filter ((== toCardCode def) . toCardCode) cards
    map
      copies
      [Enemies.darkBats, Locations.hiddenPath, Treacheries.fightingAmongFriends, Enemies.wickedSpider]
      `shouldBe` [0, 0, 0, 0]
    map copies [Enemies.fatSpider, Locations.giantWeb] `shouldBe` [3, 3]
    map
      copies
      [ Enemies.attercop
      , Enemies.venomousSpider
      , Locations.darkColony
      , Locations.mirkwoodPath
      , Locations.patchOfMidnight
      , Treacheries.aHorribleBattle
      , Treacheries.spiderBite
      ]
      `shouldBe` [3, 5, 2, 2, 3, 2, 2]
    getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
      `shouldReturn` [toCardCode Locations.theSpidersGlade]

  it "leaves the standard encounter deck unchanged" . gameTestIn fliesAndSpiders testDeck $ \_ -> do
    run $ DoStep 1 Setup
    cards <- getEncounterDeck
    length cards `shouldBe` 38
    length (filter ((== toCardCode Enemies.wickedSpider) . toCardCode) cards) `shouldBe` 5

  it "gives a chosen hero 1 poison at nightmare setup" . nightmare $ \pid -> do
    run $ DoStep 8 Setup
    h <- hero "01012"
    chooseTarget pid h
    poisonOn h `shouldReturn` 1
    clean <- hero "01002"
    poisonOn clean `shouldReturn` 0

  it "gives every player a separate setup poison choice" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    run $ DoStep 8 Setup
    heroes <- getsGame (toList . (.entities.heroes))
    let owned pid = toTarget $ fromJustNote "hero" $ find (\h -> h.owner == pid && toCardCode h == "01012") heroes
    chooseTarget one (owned one)
    chooseTarget two (owned two)
    poisonOn (owned one) `shouldReturn` 1
    poisonOn (owned two) `shouldReturn` 1

  it "reduces Attercop's engagement cost by poison cards, not poisoned characters" . nightmare $ \_ -> do
    e <- stageEnemy Enemies.attercop
    h <- hero "01012"
    run $ GivePoison h 2
    run . (`GivePoison` 1) =<< hero "01002"
    getEngagementCost e `shouldReturn` 32
    run $ DiscardPoison h 1
    getEngagementCost e `shouldReturn` 33

  it "prevents poisoned heroes and allies from fighting Attercop alone" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.attercop
    h <- hero "01012"
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ GivePoison h 1
    run $ GivePoison (toTarget ally) 1
    getModifiers h >>= (`shouldSatisfy` elem (CannotFight (EnemyWithId e)))
    getModifiers (toTarget ally) >>= (`shouldSatisfy` elem (CannotFight (EnemyWithId e)))
    clean <- hero "01002"
    getModifiers clean >>= (`shouldSatisfy` notElem (CannotFight (EnemyWithId e)))

  it "gives Venomous Spider's declared defender 1 poison" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.venomousSpider
    run $ EngageEnemy pid e
    h <- hero "01012"
    run $ CheckWindows [mkAfter $ DeclaredAsDefender h e]
    poisonOn h `shouldReturn` 1

  it "gives Venomous Spider's shadow defender 2 poison, and does nothing undefended" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.darkBats
    h <- hero "01012"
    run . AttachShadowCard e =<< genEncounterCard Enemies.venomousSpider
    run $ ResolveShadow e pid [h]
    poisonOn h `shouldReturn` 2
    run . AttachShadowCard e =<< genEncounterCard Enemies.venomousSpider
    run $ ResolveShadow e pid []
    poisonOn h `shouldReturn` 2

  it "charges Dark Colony's travel poison to a chosen hero" . nightmare $ \pid -> do
    l <- stageLocation Locations.darkColony
    run $ TravelTo l
    h <- hero "01012"
    chooseTarget pid h
    poisonOn h `shouldReturn` 1
    (.placement) <$> getLocation l `shouldReturn` ActiveLocation

  it "prevents poisoned characters from readying only while Dark Colony is active" . nightmare $ \_ -> do
    h <- hero "01012"
    run $ GivePoison h 1
    l <- stageLocation Locations.darkColony
    getModifiers h >>= (`shouldSatisfy` notElem CannotReady)
    run $ PlaceLocation l ActiveLocation
    run $ Exhaust h
    run $ Ready h
    (.heroExhausted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    clean <- hero "01002"
    getModifiers clean >>= (`shouldSatisfy` notElem CannotReady)
    run $ PlaceLocation l StagingArea
    run $ Ready h
    (.heroExhausted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it "makes Mirkwood Path's threat equal to all poison in play" . nightmare $ \pid -> do
    l <- stageLocation Locations.mirkwoodPath
    getLocationThreat l `shouldReturn` 0
    run . (`GivePoison` 2) =<< hero "01012"
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ GivePoison (toTarget ally) 1
    getLocationThreat l `shouldReturn` 3

  it "makes a hero unconscious as Mirkwood Path is explored" . nightmare $ \pid -> do
    l <- stageLocation Locations.mirkwoodPath
    run $ PlaceTokens GameSource (LocationTarget l) Progress 3
    h <- hero "01012"
    chooseTarget pid h
    (.heroUnconscious) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "adds attack per poisoned character for Mirkwood Path's shadow" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.darkBats
    attack <- getEnemyAttack e
    run . (`GivePoison` 2) =<< hero "01012"
    run . (`GivePoison` 1) =<< hero "01002"
    run . AttachShadowCard e =<< genEncounterCard Locations.mirkwoodPath
    run $ ResolveShadow e pid []
    getEnemyAttack e `shouldReturn` (attack + 2)

  it "blanks only poisoned characters while Patch of Midnight is active" . nightmare $ \_ -> do
    h <- hero "01012"
    run $ GivePoison h 1
    l <- stageLocation Locations.patchOfMidnight
    getModifiers h >>= (`shouldSatisfy` notElem Blank)
    run $ PlaceLocation l ActiveLocation
    getModifiers h >>= (`shouldSatisfy` elem Blank)
    clean <- hero "01002"
    getModifiers clean >>= (`shouldSatisfy` notElem Blank)
    run $ DiscardPoison h 1
    getModifiers h >>= (`shouldSatisfy` notElem Blank)

  it "raises threat per poisoned character for Patch of Midnight's shadow" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.darkBats
    before <- (.threat) <$> getPlayer pid
    run . (`GivePoison` 2) =<< hero "01012"
    run . (`GivePoison` 1) =<< hero "01002"
    run . AttachShadowCard e =<< genEncounterCard Locations.patchOfMidnight
    run $ ResolveShadow e pid []
    (.threat) <$> getPlayer pid `shouldReturn` (before + 2)

  it "allows A Horrible Battle to poison every hero instead of revealing a card" . nightmare $ \pid -> do
    reveal Treacheries.aHorribleBattle
    chooseLabel "$label.aHorribleBattle.poison" pid
    for_ ["01002", "01007", "01012"] \code -> hero code >>= \h -> poisonOn h `shouldReturn` 1

  it "allows A Horrible Battle to reveal an encounter card instead" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Treacheries.aHorribleBattle, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.aHorribleBattle.reveal" pid
    resolveRevealed
    _ <- locationOf Locations.hiddenPath
    h <- hero "01012"
    poisonOn h `shouldReturn` 0

  it "attaches Spider Bite to a conscious hero, counts as a Condition, and surges" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Treacheries.spiderBite, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    h <- hero "01012"
    chooseTarget pid h
    resolveRevealed
    attachedWithTrait Condition h >>= (`shouldSatisfy` (not . null))
    _ <- locationOf Locations.hiddenPath
    pure ()

  it "has Spider Bite poison its attached hero at the end of refresh" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Treacheries.spiderBite, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    h <- hero "01012"
    chooseTarget pid h
    resolveRevealed
    run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
    poisonOn h `shouldReturn` 1

  it "discards Spider Bite when its hero becomes unconscious" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Treacheries.spiderBite, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    h <- hero "01012"
    chooseTarget pid h
    resolveRevealed
    run $ MakeUnconscious h
    attachedWithTrait Condition h `shouldReturn` []
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "lets the selected player pay Dark Colony's travel cost with their own hero" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    l <- stageLocation Locations.darkColony
    heroes <- getsGame (toList . (.entities.heroes))
    let owned pid = toTarget $ fromJustNote "hero" $ find (\h -> h.owner == pid && toCardCode h == "01012") heroes
    run $ TravelTo l
    chooseTarget one two
    chooseTarget two (owned two)
    poisonOn (owned two) `shouldReturn` 1
    poisonOn (owned one) `shouldReturn` 0

  it "keeps Dark Colony's poison payment in the location's staging area" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    heroes <- getsGame (toList . (.entities.heroes))
    let owned pid = toTarget $ fromJustNote "hero" $ find (\h -> h.owner == pid && toCardCode h == "01012") heroes
    run $ SeparatePlayers [[one], [two]]
    run $ FocusArea one
    l <- stageLocation Locations.darkColony
    run $ TravelTo l
    chooseTarget one (owned one)
    poisonOn (owned one) `shouldReturn` 1
    run $ FocusArea two
    poisonOn (owned two) `shouldReturn` 0

  it "allows every player to make their own A Horrible Battle choice" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    _ <- setEncounterDeck [Treacheries.aHorribleBattle, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.aHorribleBattle.poison" one
    chooseLabel "$label.aHorribleBattle.reveal" two
    resolveRevealed
    heroes <- getsGame (toList . (.entities.heroes))
    for_ heroes \h -> poisonOn (toTarget h) `shouldReturn` (if h.owner == one then 1 else 0)

  it "makes the shadow of A Horrible Battle cause an additional attack" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid e
    run . AttachShadowCard e =<< genEncounterCard Treacheries.aHorribleBattle
    _ <- setEncounterDeck [Locations.hiddenPath]
    run $ EnemyAttack pid e
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    h <- hero "01002"
    chooseTarget pid h
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    other <- hero "01012"
    chooseTarget pid other
    getDamage h `shouldReturn` 2
    getDamage other `shouldReturn` 2

  it "does not offer an unconscious hero or a second Spider Bite on the same hero" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Treacheries.spiderBite, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    h <- hero "01012"
    chooseTarget pid h
    resolveRevealed
    run . MakeUnconscious =<< hero "01002"
    _ <- setEncounterDeck [Treacheries.spiderBite, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    -- Only the third hero remains eligible, so it is selected automatically.
    other <- hero "01007"
    attachedWithTrait Condition other >>= (`shouldSatisfy` (not . null))
    attachedWithTrait Condition h >>= (\xs -> length xs `shouldBe` 1)

  it "makes Venomous Spider's attack undefended if its Forced poison incapacitates the defender" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.venomousSpider
    run $ EngageEnemy pid e
    defender <- hero "01012"
    run $ GivePoison defender 3
    recipient <- hero "01002"
    _ <- putAttachmentOn pid Attachments.citadelPlate recipient
    run $ EnemyAttack pid e
    passActions pid
    chooseTarget pid defender
    passActions pid
    chooseTarget pid recipient
    passActions pid
    (.heroUnconscious) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    getDamage defender `shouldReturn` 0
    getDamage recipient `shouldReturn` 4

  it "makes an attack undefended if Venomous Spider's shadow incapacitates its defender" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid e
    defender <- hero "01007"
    run $ GivePoison defender 1
    run . AttachShadowCard e =<< genEncounterCard Enemies.venomousSpider
    run $ EnemyAttack pid e
    passActions pid
    chooseTarget pid defender
    passActions pid
    recipient <- hero "01002"
    chooseTarget pid recipient
    passActions pid
    (.heroUnconscious) . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
    getDamage defender `shouldReturn` 0
    getDamage recipient `shouldReturn` 2

  it "does not restore a defense when the unconscious defender is readied before damage" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid e
    defender <- hero "01012"
    run $ Run [MakeUnconscious defender, ReadyUnconscious defender, AttackDamage e defender]
    recipient <- hero "01002"
    chooseTarget pid recipient
    getDamage defender `shouldReturn` 0
    getDamage recipient `shouldReturn` 2

  it "keeps a conscious co-defender when another defender becomes unconscious" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid e
    gone <- hero "01012"
    remaining <- hero "01007"
    run $ Run [MakeUnconscious gone, DefendedByMany pid e [gone, remaining]]
    getDamage gone `shouldReturn` 0
    getDamage remaining `shouldReturn` 1

  it "uses Patch of Midnight's printed threat and quest points" . nightmare $ \_ -> do
    l <- stageLocation Locations.patchOfMidnight
    getLocationThreat l `shouldReturn` 3
    (.questPoints) <$> getLocation l `shouldReturn` 5

  it "forbids traveling to Dark Colony when no player can supply a poison card" . nightmare $ \pid -> do
    putGame . (playersL . ix pid . deckL .~ []) =<< getGame
    l <- stageLocation Locations.darkColony
    getModifiers (LocationTarget l) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  it "removes an unconscious character from the quest and from an unresolved attack" . nightmare $ \_ -> do
    h <- hero "01012"
    run $ CommitToQuest h
    run $ MakeUnconscious h
    (.heroCommitted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
    run $ ReadyUnconscious h
    run $ AttackWith h
    run $ MakeUnconscious h
    (.heroAttacking) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it
    "assigns an undefended staging enemy's damage to the attacked player when their defender becomes unconscious" . twoPlayerTestIn nightmareScenario testDeck $ \_ two -> do
    heroes <- getsGame (toList . (.entities.heroes))
    let owned code = toTarget $ fromJustNote "hero" $ find (\h -> h.owner == two && toCardCode h == code) heroes
        defender = owned "01012"
        recipient = owned "01002"
    e <- stageEnemy Enemies.venomousSpider
    run $ GivePoison defender 3
    _ <- putAttachmentOn two Attachments.citadelPlate recipient
    run $ EnemyAttack two e
    passTableActions
    chooseTarget two defender
    passTableActions
    chooseTarget two recipient
    passTableActions
    getDamage defender `shouldReturn` 0
    getDamage recipient `shouldReturn` 4

  it "applies nightmare setup poison after drawing the starting hand and completing quest setup" . nightmare $ \pid -> do
    run Setup
    h <- hero "01012"
    poisonOn h `shouldReturn` 0
    chooseLabel "$label.keepHand" pid
    _ <- setEncounterDeck [Locations.hiddenPath]
    chooseOptionMatching "perform quest setup" pid (const True)
    chooseLabel "$label.intoMirkwood.leaveOut" pid
    poisonOn h `shouldReturn` 0
    chooseTarget pid h
    poisonOn h `shouldReturn` 1
    (.hand) <$> getPlayer pid >>= (\cards -> length cards `shouldBe` 6)

  it "reveals a card for A Horrible Battle if the player cannot supply poison for every hero" . nightmare $ \pid -> do
    putGame . (playersL . ix pid . deckL %~ take 1) =<< getGame
    _ <- setEncounterDeck [Treacheries.aHorribleBattle, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
    _ <- locationOf Locations.hiddenPath
    for_ ["01002", "01007", "01012"] \code -> hero code >>= \h -> poisonOn h `shouldReturn` 0
