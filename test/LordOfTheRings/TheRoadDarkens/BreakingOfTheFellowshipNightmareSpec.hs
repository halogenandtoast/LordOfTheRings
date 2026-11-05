module LordOfTheRings.TheRoadDarkens.BreakingOfTheFellowshipNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy, getMainQuestProgress)
import LordOfTheRings.Scenario (breakingOfTheFellowship, modeL, GameMode (NightmareMode), scenarioSetAside)
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Helpers.Window (travelableLocations, getAllAbilities, getActionAbilities, actionsAndResponsesAllowed)
import LordOfTheRings.Ability (isForcedAbility)
import LordOfTheRings.Modifier (ModifierType (CannotReady, Blank, GainTrait, GainKeyword, CannotEngagePlayer, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy, CannotDiscardFromHand))
import LordOfTheRings.Keyword (Keyword (Toughness))
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Window (WindowType (RoundEnds, DiscardedFromDeck, Revealed), mkWhen, mkAfter)
import TestImport

botf :: (PlayerId -> GameT ()) -> IO ()
botf = gameTestIn breakingOfTheFellowship testDeck

botfTwo :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
botfTwo = twoPlayerTestIn breakingOfTheFellowship testDeck

botfThree :: (PlayerId -> PlayerId -> PlayerId -> GameT ()) -> IO ()
botfThree body = do
  seats <- traverse (\name -> either (error . show) id <$> loadPlayer name testDeck) ("one" :| ["two", "three"])
  env <- newGameEnv (newTableIn breakingOfTheFellowship seats) (const $ pure ()) 1
  runGameT env $ getsGame (.playerOrder) >>= \case
    [one, two, three] -> body one two three
    _ -> error "three players"

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations))

riverAtThisStage :: GameT LocationId
riverAtThisStage = getsGame (toId . fromJustNote "Langflood River" . find ((== toCardCode Locations.langfloodRiver) . toCardCode) . toList . (.entities.locations))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (toId . fromJustNote "staged enemy" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

beravorOf :: PlayerId -> GameT Target
beravorOf pid = getsGame (toTarget . fromJustNote "Beravor" . find (\h -> toCardCode h == "01012" && heroOwner (toAttrs h) == pid) . toList . (.entities.heroes))

passTableActions :: PlayerId -> PlayerId -> GameT ()
passTableActions one two = do
  questions <- traverse getQuestion [one, two]
  let action = \case
        Just (QuestionLabel prompt _ _ _) -> prompt `elem` ["$label.takeAction", "$label.planningActions", "$label.noRemainingActions"]
        _ -> False
  when (any action questions) do
    passActions one
    passActions two
    passTableActions one two

revealTreachery :: CardDef -> GameT ()
revealTreachery def = do
  _ <- setEncounterDeck [def]
  run RevealEncounterCard
  resolveRevealed

revealHunting :: GameT ()
revealHunting = do
  _ <- setEncounterDeck [Treacheries.huntingTheRing, Locations.theOldFord]
  run RevealEncounterCard
  resolveRevealed

huntingCards :: GameT [(TreacheryId, Placement)]
huntingCards = getsGame (map (\t -> (toId t, t.placement)) . filter ((== toCardCode Treacheries.huntingTheRing) . toCardCode) . toList . (.entities.treacheries))

resolveForcedChoices :: PlayerId -> GameT ()
resolveForcedChoices pid = getQuestion pid >>= \case
  Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
    chooseOptionMatching "resolve a forced effect" pid (\case AbilityLabel {} -> True; _ -> False)
    resolveForcedChoices pid
  _ -> pure ()

spec :: Spec
spec = describe "Breaking of the Fellowship Nightmare" do
  it "builds the Nightmare encounter deck with the exact removals and nineteen new cards" . gameTestIn (overAttrs (modeL .~ NightmareMode) breakingOfTheFellowship) testDeck $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.urukHaiArcher, Locations.riverAnduin, Locations.slopesOfAmonHen, Treacheries.blackFeatheredArrows]
      `shouldBe` [0, 2, 2, 1]
    map copies [Enemies.isengardArcher, Enemies.isengardTracker, Locations.langfloodRiver, Locations.westernEmynMuil, Treacheries.arrowsInTheNight, Treacheries.huntingTheRing, Treacheries.discordAndDistrust]
      `shouldBe` [3, 4, 3, 3, 2, 2, 2]
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    sort aside `shouldBe` sort (map toCardCode [Objectives.frodosChoice, Locations.seatOfSeeing, Locations.parthGalen, Locations.theArgonath, Locations.sarnGebir])

  it "Discord and Distrust prevents in-play actions until the end of the round" . botf $ \pid -> do
    getActionAbilities pid >>= (`shouldSatisfy` notNull)
    threatBefore <- (.threat) <$> getPlayer pid
    revealTreachery Treacheries.discordAndDistrust
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 2)
    actionsAndResponsesAllowed `shouldReturn` False
    getActionAbilities pid `shouldReturn` []
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    getActionAbilities pid >>= (`shouldSatisfy` notNull)

  it "Discord and Distrust blocks action events but still permits playing allies" . botf $ \pid -> do
    hero <- beravorOf pid
    run $ PlaceTokens GameSource hero Resource 10
    eventCard <- putCardIntoHand pid Events.daeronsRunes
    allyCard <- putCardIntoHand pid Allies.henamarthRiversong
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player eventCard `shouldReturn` True
    canPlay player allyCard `shouldReturn` True
    revealTreachery Treacheries.discordAndDistrust
    canPlay player eventCard `shouldReturn` False
    canPlay player allyCard `shouldReturn` True
    run $ EndEffects UntilEndOfRound
    canPlay player eventCard `shouldReturn` True

  it "Discord and Distrust prevents actions from hand" . botf $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    bofurCard <- putCardIntoHand pid Allies.bofur
    putGame . (phaseL .~ QuestPhase) =<< getGame
    player <- getPlayer pid
    handActions player >>= (`shouldSatisfy` elem bofurCard)
    revealTreachery Treacheries.discordAndDistrust
    handActions player `shouldReturn` []
    run $ EndEffects UntilEndOfRound
    handActions player >>= (`shouldSatisfy` elem bofurCard)

  it "Discord and Distrust can itself be canceled before its restriction applies" . botf $ \pid -> do
    cancellationCard <- putCardIntoHand pid Events.aTestOfWill
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    _ <- setEncounterDeck [Treacheries.discordAndDistrust]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget cancellationCard.id)
    resolveRevealed
    actionsAndResponsesAllowed `shouldReturn` True
    getActionAbilities pid >>= (`shouldSatisfy` notNull)

  it "Discord and Distrust prevents response events from hand" . botf $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    revealTreachery Treacheries.discordAndDistrust
    eid <- stage Enemies.zirakzigilArcher
    _ <- putCardIntoHand pid Events.aTestOfWill
    let windows = [mkWhen $ Revealed $ EnemyTarget eid]
    handResponses pid windows (DoStep 1 Setup) `shouldReturn` []
    run $ EndEffects UntilEndOfRound
    handResponses pid windows (DoStep 1 Setup) >>= (`shouldSatisfy` notNull)

  it "Discord and Distrust prevents responses from the discard pile" . botf $ \pid -> do
    miner <- putCardIntoHand pid Allies.eredLuinMiner
    run $ DiscardFromHand pid miner.id
    let windows = [mkAfter $ DiscardedFromDeck pid miner.id]
    discardResponses pid windows [] (const $ DoStep 1 Setup) >>= (`shouldSatisfy` notNull)
    revealTreachery Treacheries.discordAndDistrust
    discardResponses pid windows [] (const $ DoStep 1 Setup) `shouldReturn` []
    run $ EndEffects UntilEndOfRound
    discardResponses pid windows [] (const $ DoStep 1 Setup) >>= (`shouldSatisfy` notNull)

  it "Discord and Distrust preserves Forced effects" . botf $ \pid -> do
    revealTreachery Treacheries.discordAndDistrust
    getAllAbilities >>= (`shouldSatisfy` all isForcedAbility)
    hero <- beravorOf pid
    _ <- stage Enemies.isengardArcher
    chooseTarget pid hero
    getDamage hero `shouldReturn` 2

  it "Discord and Distrust applies across separated staging areas" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    revealTreachery Treacheries.discordAndDistrust
    getActionAbilities one `shouldReturn` []
    run $ FocusArea two
    actionsAndResponsesAllowed `shouldReturn` False
    getActionAbilities two `shouldReturn` []
    run $ EndEffects UntilEndOfRound
    getActionAbilities two >>= (`shouldSatisfy` notNull)

  it "Discord and Distrust's shadow stops its defender readying only for this round" . botf $ \pid -> do
    hero <- beravorOf pid
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.discordAndDistrust
    run $ ResolveShadow eid pid [hero]
    getModifiers hero >>= (`shouldSatisfy` elem CannotReady)
    actionsAndResponsesAllowed `shouldReturn` True
    run $ EndEffects UntilEndOfRound
    getModifiers hero >>= (`shouldSatisfy` notElem CannotReady)

  it "Isengard Archer damages a character when staged and has Archery 2 and Toughness 1" . botf $ \pid -> do
    hero <- beravorOf pid
    eid <- stage Enemies.isengardArcher
    chooseTarget pid hero
    getDamage hero `shouldReturn` 2
    getEngagementCost eid `shouldReturn` 44
    getArcheryTotal `shouldReturn` 2
    run $ DealDamage GameSource (EnemyTarget eid) 2
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Isengard Archer damages each player at a shared stage" . botfTwo $ \one two -> do
    heroOne <- beravorOf one
    heroTwo <- beravorOf two
    _ <- stage Enemies.isengardArcher
    chooseTarget one heroOne
    chooseTarget two heroTwo
    getDamage heroOne `shouldReturn` 2
    getDamage heroTwo `shouldReturn` 2

  it "Isengard Archer only damages players at its own separate stage" . botfTwo $ \one two -> do
    heroOne <- beravorOf one
    heroTwo <- beravorOf two
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    _ <- stage Enemies.isengardArcher
    getQuestion one >>= (`shouldSatisfy` isNothing)
    chooseTarget two heroTwo
    getDamage heroOne `shouldReturn` 0
    getDamage heroTwo `shouldReturn` 2

  it "Isengard Archer's shadow damages an ally and does not damage a hero" . botf $ \pid -> do
    hero <- beravorOf pid
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.isengardArcher
    run $ ResolveShadow eid pid []
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    getDamage hero `shouldReturn` 0
    map toCardCode . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.guardOfTheCitadel))

  it "Isengard Archer's shadow does nothing when its defending player controls no allies" . botf $ \pid -> do
    hero <- beravorOf pid
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.isengardArcher
    run $ ResolveShadow eid pid []
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getDamage hero `shouldReturn` 0

  it "Isengard Tracker has Archery 1 and Toughness 2" . botf $ \_ -> do
    eid <- stage Enemies.isengardTracker
    getEngagementCost eid `shouldReturn` 35
    getArcheryTotal `shouldReturn` 1
    run $ DealDamage GameSource (EnemyTarget eid) 3
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Isengard Tracker returns to staging after destroying a character" . botf $ \pid -> do
    eid <- stage Enemies.isengardTracker
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  it "Isengard Tracker remains engaged when its defender survives" . botf $ \pid -> do
    eid <- stage Enemies.isengardTracker
    run $ EngageEnemy pid eid
    defender <- beravorOf pid
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    (.placement) <$> getEnemy eid `shouldReturn` EngagedWith pid

  it "Isengard Tracker moves from another stage to the first player's staging area after a kill" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    eid <- stage Enemies.isengardTracker
    run $ EngageEnemy two eid
    defender <- putAllyIntoPlay two Allies.guardOfTheCitadel
    run $ EnemyAttack two eid
    passTableActions one two
    chooseTarget two defender
    passTableActions one two
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    run $ FocusArea one
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  it "Isengard Tracker's shadow moves its attacker to the first player's stage even without a kill" . botfTwo $ \one two -> do
    defender <- beravorOf two
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    eid <- stage Enemies.goblinSniper
    run $ EngageEnemy two eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.isengardTracker
    run $ EnemyAttack two eid
    passTableActions one two
    chooseTarget two defender
    passTableActions one two
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    run $ FocusArea one
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  it "Isengard Archer triggers again when an engaged Archer returns to staging" . botf $ \pid -> do
    hero <- beravorOf pid
    eid <- stage Enemies.isengardArcher
    chooseTarget pid hero
    run $ RemoveTokens GameSource hero Damage 2
    run $ EngageEnemy pid eid
    run $ PlaceEnemy eid StagingArea
    chooseTarget pid hero
    getDamage hero `shouldReturn` 2
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Isengard Archer triggers for the destination players when moved between staging areas" . botfTwo $ \one two -> do
    heroOne <- beravorOf one
    heroTwo <- beravorOf two
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    eid <- stage Enemies.isengardArcher
    chooseTarget two heroTwo
    run $ PlaceInStagingAreaOf one (EnemyTarget eid)
    chooseTarget one heroOne
    getDamage heroOne `shouldReturn` 2
    getDamage heroTwo `shouldReturn` 2
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    run $ FocusArea one
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
    getQuestion one >>= (`shouldSatisfy` isNothing)

  it "moves an Archer from an area out of focus, discards its shadows, and resolves entry damage at its destination" . botfTwo $ \one two -> do
    heroOne <- beravorOf one
    heroTwo <- beravorOf two
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    eid <- stage Enemies.isengardArcher
    chooseTarget two heroTwo
    shadow <- genEncounterCard Enemies.forestSpider
    run $ AttachShadowCard eid shadow
    run $ FocusArea one
    run $ PlaceInStagingAreaOf one (EnemyTarget eid)
    chooseTarget one heroOne
    getDamage heroOne `shouldReturn` 2
    getDamage heroTwo `shouldReturn` 2
    length . (.shadowCards) <$> getEnemy eid `shouldReturn` 0
    map (.id) <$> getEncounterDiscard >>= (`shouldSatisfy` elem shadow.id)
    run $ FocusArea two
    getsGame (length . (.entities.enemies)) `shouldReturn` 0

  it "does not retrigger an Archer or discard its shadows when it already occupies the destination area out of focus" . botfTwo $ \one two -> do
    heroTwo <- beravorOf two
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    eid <- stage Enemies.isengardArcher
    chooseTarget two heroTwo
    shadow <- genEncounterCard Enemies.forestSpider
    run $ AttachShadowCard eid shadow
    run $ FocusArea one
    run $ PlaceInStagingAreaOf two (EnemyTarget eid)
    getQuestion two >>= (`shouldSatisfy` isNothing)
    getDamage heroTwo `shouldReturn` 2
    getEncounterDiscard `shouldReturn` []
    run $ FocusArea two
    map (.id) . (.shadowCards) <$> getEnemy eid `shouldReturn` [shadow.id]

  it "Langflood River caps total progress placed each round, including progress subsequently removed" . botf $ \_ -> do
    q <- putQuestStage Quests.theLongDarkOfMoria
    _ <- stageLocation Locations.langfloodRiver
    run $ PlaceTokens GameSource q Progress 3
    run $ PlaceTokens GameSource q Progress 3
    getMainQuestProgress `shouldReturn` 4
    run $ RemoveTokens GameSource q Progress 4
    run $ PlaceTokens GameSource q Progress 2
    getMainQuestProgress `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource q Progress 5
    getMainQuestProgress `shouldReturn` 4

  it "Langflood River counts progress placed before it entered staging and stops limiting the quest when active" . botf $ \_ -> do
    q <- putQuestStage Quests.theLongDarkOfMoria
    run $ PlaceTokens GameSource q Progress 3
    lid <- stageLocation Locations.langfloodRiver
    run $ PlaceTokens GameSource q Progress 3
    getMainQuestProgress `shouldReturn` 4
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceTokens GameSource q Progress 2
    getMainQuestProgress `shouldReturn` 6

  it "Langflood River limits only its own separate quest stage" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea one
    qOne <- putQuestStage Quests.theLongDarkOfMoria
    _ <- stageLocation Locations.langfloodRiver
    run $ PlaceTokens GameSource qOne Progress 8
    getMainQuestProgress `shouldReturn` 4
    run $ FocusArea two
    qTwo <- putQuestStage Quests.theLongDarkOfMoria
    run $ PlaceTokens GameSource qTwo Progress 8
    getMainQuestProgress `shouldReturn` 8

  it "Langflood River is discarded when explored without another staging area" . botf $ \_ -> do
    lid <- stageLocation Locations.langfloodRiver
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.langfloodRiver]

  it "Langflood River leaves play when explored, then enters the area to the left with its tokens and attachments cleared" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea one
    lid <- stageLocation Locations.langfloodRiver
    cardId <- (.cardId) . toAttrs <$> getLocation lid
    _ <- putAttachmentOn one Attachments.pathOfNeed (LocationTarget lid)
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget lid) Damage 2
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    map toCardCode . (.discard) <$> getPlayer one >>= (`shouldSatisfy` elem (toCardCode Attachments.pathOfNeed))
    run $ FocusArea two
    arrived <- riverAtThisStage
    (arrived /= lid) `shouldBe` True
    location <- getLocation arrived
    (.cardId) (toAttrs location) `shouldBe` cardId
    (.placement) location `shouldBe` StagingArea
    (.progress) location `shouldBe` 0
    getDamage (LocationTarget arrived) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Locations.langfloodRiver))

  it "Langflood River moves from the last staging area back to the first" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    lid <- stageLocation Locations.langfloodRiver
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    run $ FocusArea one
    arrived <- riverAtThisStage
    (.placement) <$> getLocation arrived `shouldReturn` StagingArea

  it "discarding Langflood River without exploring it does not move it to another stage" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea one
    lid <- stageLocation Locations.langfloodRiver
    run $ Discard GameSource (LocationTarget lid)
    run $ FocusArea two
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.langfloodRiver]

  it "Langflood River's progress cap follows it to the destination stage after exploration" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    qTwo <- putQuestStage Quests.theLongDarkOfMoria
    run $ FocusArea one
    qOne <- putQuestStage Quests.theLongDarkOfMoria
    lid <- stageLocation Locations.langfloodRiver
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
    run $ PlaceTokens GameSource qOne Progress 8
    getMainQuestProgress `shouldReturn` 8
    run $ FocusArea two
    run $ PlaceTokens GameSource qTwo Progress 8
    getMainQuestProgress `shouldReturn` 4

  it "Langflood River moves one area to the left, rather than to the first player or the other neighbor" . botfThree $ \one two three -> do
    run $ SeparateStagingAreas [[one], [two], [three]]
    run $ FocusArea two
    lid <- stageLocation Locations.langfloodRiver
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
    run $ FocusArea one
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    run $ FocusArea three
    arrived <- riverAtThisStage
    (.placement) <$> getLocation arrived `shouldReturn` StagingArea

  it "Western Emyn Muil gives staged and engaged Uruk-hai Toughness, but not other enemies, only while staged" . botf $ \pid -> do
    lid <- stageLocation Locations.westernEmynMuil
    uruk <- stage Enemies.isengardTracker
    other <- stage Enemies.forestSpider
    run $ EngageEnemy pid uruk
    run $ DealDamage GameSource (EnemyTarget uruk) 3
    getDamage (EnemyTarget uruk) `shouldReturn` 0
    run $ DealDamage GameSource (EnemyTarget other) 2
    getDamage (EnemyTarget other) `shouldReturn` 2
    run $ PlaceLocation lid ActiveLocation
    run $ DealDamage GameSource (EnemyTarget uruk) 3
    getDamage (EnemyTarget uruk) `shouldReturn` 1

  it "Western Emyn Muil's Toughness bonus stacks across copies" . botf $ \_ -> do
    _ <- stageLocation Locations.westernEmynMuil
    _ <- stageLocation Locations.westernEmynMuil
    uruk <- stage Enemies.isengardTracker
    run $ DealDamage GameSource (EnemyTarget uruk) 4
    getDamage (EnemyTarget uruk) `shouldReturn` 0
    run $ DealDamage GameSource (EnemyTarget uruk) 5
    getDamage (EnemyTarget uruk) `shouldReturn` 1

  it "Western Emyn Muil requires engaging the highest current Toughness enemy before becoming active" . botf $ \pid -> do
    lid <- stageLocation Locations.westernEmynMuil
    strongest <- stage Enemies.isengardTracker
    weaker <- stage Enemies.forestSpider
    run $ TravelTo lid
    (.placement) <$> getEnemy strongest `shouldReturn` EngagedWith pid
    (.placement) <$> getEnemy weaker `shouldReturn` StagingArea
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Western Emyn Muil allows choosing among ties, counting its own bonus before it becomes active" . botf $ \pid -> do
    lid <- stageLocation Locations.westernEmynMuil
    uruk <- stage Enemies.isengardTracker
    other <- stage Enemies.forestSpider
    run $ CreateEffect $ Effect GameSource (EnemyTarget other) [GainKeyword $ Toughness 3] UntilEndOfRound
    run $ TravelTo lid
    chooseTarget pid uruk
    (.placement) <$> getEnemy uruk `shouldReturn` EngagedWith pid
    (.placement) <$> getEnemy other `shouldReturn` StagingArea
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Western Emyn Muil cannot be traveled to with no enemy to engage" . botf $ \_ -> do
    lid <- stageLocation Locations.westernEmynMuil
    travelableLocations >>= (`shouldSatisfy` notElem lid)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea

  it "Western Emyn Muil cannot substitute a weaker enemy when the toughest cannot engage the player" . botf $ \pid -> do
    lid <- stageLocation Locations.westernEmynMuil
    strongest <- stage Enemies.isengardTracker
    _ <- stage Enemies.forestSpider
    run $ CreateEffect $ Effect GameSource (EnemyTarget strongest) [CannotEngagePlayer pid] UntilEndOfRound
    travelableLocations >>= (`shouldSatisfy` notElem lid)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea
    (.placement) <$> getEnemy strongest `shouldReturn` StagingArea

  it "Western Emyn Muil cannot engage a toughest enemy that cannot leave staging" . botf $ \_ -> do
    lid <- stageLocation Locations.westernEmynMuil
    strongest <- stage Enemies.isengardTracker
    run $ CreateEffect $ Effect GameSource (EnemyTarget strongest) [CannotLeaveStagingArea] UntilEndOfRound
    travelableLocations >>= (`shouldSatisfy` notElem lid)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea

  it "Western Emyn Muil's forced engagement ignores optional-engagement restrictions" . botf $ \pid -> do
    lid <- stageLocation Locations.westernEmynMuil
    strongest <- stage Enemies.isengardTracker
    run $ CreateEffect $ Effect GameSource (EnemyTarget strongest) [CannotBeOptionallyEngagedBy AnyPlayer] UntilEndOfRound
    travelableLocations >>= (`shouldSatisfy` elem lid)
    run $ TravelTo lid
    (.placement) <$> getEnemy strongest `shouldReturn` EngagedWith pid

  it "Western Emyn Muil's travel cost is paid by a player at its own separate stage" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    lid <- stageLocation Locations.westernEmynMuil
    strongest <- stage Enemies.isengardTracker
    run $ TravelTo lid
    (.placement) <$> getEnemy strongest `shouldReturn` EngagedWith two
    getQuestion one >>= (`shouldSatisfy` isNothing)
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Western Emyn Muil does not toughen an Uruk-hai engaged at another stage" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    uruk <- stage Enemies.isengardTracker
    run $ EngageEnemy two uruk
    run $ FocusArea one
    _ <- stageLocation Locations.westernEmynMuil
    run $ FocusArea two
    run $ DealDamage GameSource (EnemyTarget uruk) 3
    getDamage (EnemyTarget uruk) `shouldReturn` 1

  it "a blanked Western Emyn Muil has no travel cost" . botf $ \_ -> do
    lid <- stageLocation Locations.westernEmynMuil
    eid <- stage Enemies.isengardTracker
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    travelableLocations >>= (`shouldSatisfy` elem lid)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  it "Western Emyn Muil allows either player at a shared stage to pay its travel cost" . botfTwo $ \one two -> do
    lid <- stageLocation Locations.westernEmynMuil
    strongest <- stage Enemies.isengardTracker
    run $ TravelTo lid
    chooseTarget one (PlayerTarget two)
    (.placement) <$> getEnemy strongest `shouldReturn` EngagedWith two
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Western Emyn Muil still requires an engagement when the highest Toughness is zero" . botf $ \pid -> do
    lid <- stageLocation Locations.westernEmynMuil
    eid <- stage Enemies.forestSpider
    run $ TravelTo lid
    (.placement) <$> getEnemy eid `shouldReturn` EngagedWith pid
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Arrows in the Night adds six archery for the round with an empty hand, lasting after the treachery is discarded" . botf $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    revealTreachery Treacheries.arrowsInTheNight
    getArcheryTotal `shouldReturn` 6
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    getArcheryTotal `shouldReturn` 0

  it "Arrows in the Night allows declining the optional discard" . botf $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    revealTreachery Treacheries.arrowsInTheNight
    chooseLabel "$label.doneDiscarding" pid
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    getArcheryTotal `shouldReturn` 6

  it "Arrows in the Night selects distinct cards before discarding them together" . botf $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    firstCard <- putCardIntoHand pid Events.fortuneOrFate
    secondCard <- putCardIntoHand pid Events.aTestOfWill
    kept <- putCardIntoHand pid Events.gandalfsSearch
    revealTreachery Treacheries.arrowsInTheNight
    chooseTarget pid (CardIdTarget firstCard.id)
    length . (.hand) <$> getPlayer pid `shouldReturn` 3
    question <- getQuestion pid
    let offered = maybe [] questionChoices question
    offered `shouldSatisfy` not . any (\case TargetLabel (CardIdTarget cid) _ -> cid == firstCard.id; _ -> False)
    chooseTarget pid (CardIdTarget secondCard.id)
    chooseLabel "$label.doneDiscarding" pid
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [kept.id]
    length . (.discard) <$> getPlayer pid `shouldReturn` 2
    getArcheryTotal `shouldReturn` 4

  it "Arrows in the Night can reduce existing archery by discarding more than six cards" . botf $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    _ <- stage Enemies.isengardTracker
    cards <- replicateM 8 (putCardIntoHand pid Events.fortuneOrFate) :: GameT [Card]
    revealTreachery Treacheries.arrowsInTheNight
    for_ cards (\c -> chooseTarget pid (CardIdTarget c.id))
    length . (.hand) <$> getPlayer pid `shouldReturn` 0
    length . (.discard) <$> getPlayer pid `shouldReturn` 8
    getArcheryTotal `shouldReturn` 0

  it "Arrows in the Night cannot reduce archery when discarding is prohibited" . botf $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
    revealTreachery Treacheries.arrowsInTheNight
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    getArcheryTotal `shouldReturn` 6
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Arrows in the Night cannot select an encounter card that is protected from discarding" . botf $ \pid -> do
    protected <- genEncounterCard Treacheries.theNecromancersReach
    putGame . (playersL . ix pid . handL .~ [EncounterCard protected]) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    revealTreachery Treacheries.arrowsInTheNight
    chooseTarget pid (CardIdTarget card.id)
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [protected.id]
    getArcheryTotal `shouldReturn` 5

  it "Arrows in the Night changes archery only at the revealed stage, and expires even while another area is in focus" . botfTwo $ \one two -> do
    putGame . (playersL . ix one . handL .~ []) . (playersL . ix two . handL .~ []) =<< getGame
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    run $ SetRevealer (Just two)
    revealTreachery Treacheries.arrowsInTheNight
    getArcheryTotal `shouldReturn` 6
    run $ FocusArea one
    getArcheryTotal `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ FocusArea two
    getArcheryTotal `shouldReturn` 0

  it "Arrows in the Night's shadow discards one random card without increasing archery" . botf $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.arrowsInTheNight
    run $ ResolveShadow eid pid []
    length . (.hand) <$> getPlayer pid `shouldReturn` 0
    map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [card.id]
    getArcheryTotal `shouldReturn` 0

  it "Arrows in the Night counts cards actually discarded even if Silver Harp returns one afterward" . botf $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    eowyn <- toTarget <$> getHeroWithCode "01007"
    harp <- putAttachmentOn pid Attachments.silverHarp eowyn
    firstCard <- putCardIntoHand pid Events.fortuneOrFate
    secondCard <- putCardIntoHand pid Events.gandalfsSearch
    revealTreachery Treacheries.arrowsInTheNight
    chooseTarget pid (CardIdTarget firstCard.id)
    offersAbilityOf pid harp `shouldReturn` False
    chooseTarget pid (CardIdTarget secondCard.id)
    length . (.discard) <$> getPlayer pid `shouldReturn` 2
    chooseAbilityOf pid harp
    length . (.hand) <$> getPlayer pid `shouldReturn` 1
    getArcheryTotal `shouldReturn` 4

  it "Hunting the Ring attaches as a Condition to the current quest and surges" . botf $ \_ -> do
    q <- putQuestStage Quests.theLongDarkOfMoria
    revealHunting
    cards <- huntingCards
    map snd cards `shouldBe` [AttachedTo q]
    for_ cards (\(tid, _) -> getModifiers (TreacheryTarget tid) >>= (`shouldSatisfy` elem (GainTrait Condition)))
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.theOldFord]

  it "Hunting the Ring raises threat for staged enemies, excludes engaged enemies, and discards itself" . botf $ \pid -> do
    _ <- putQuestStage Quests.theLongDarkOfMoria
    _ <- stage Enemies.forestSpider
    engaged <- stage Enemies.isengardTracker
    run $ EngageEnemy pid engaged
    revealHunting
    beforeThreat <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen RoundEnds]
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 1)
    huntingCards `shouldReturn` []
    (.placement) <$> getEnemy engaged `shouldReturn` EngagedWith pid

  it "Hunting the Ring raises each player's threat at a shared stage" . botfTwo $ \one two -> do
    _ <- putQuestStage Quests.theLongDarkOfMoria
    _ <- stage Enemies.forestSpider
    _ <- stage Enemies.goblinSniper
    revealHunting
    beforeOne <- (.threat) <$> getPlayer one
    beforeTwo <- (.threat) <$> getPlayer two
    run $ CheckWindows [mkWhen RoundEnds]
    (.threat) <$> getPlayer one `shouldReturn` (beforeOne + 2)
    (.threat) <$> getPlayer two `shouldReturn` (beforeTwo + 2)
    huntingCards `shouldReturn` []

  it "Hunting the Ring discards itself at round end even if no enemies are staged" . botf $ \pid -> do
    _ <- putQuestStage Quests.theLongDarkOfMoria
    revealHunting
    beforeThreat <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen RoundEnds]
    (.threat) <$> getPlayer pid `shouldReturn` beforeThreat
    huntingCards `shouldReturn` []

  it "Hunting the Ring affects only its own stage and moves those staged enemies to the first player's area" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    _ <- putQuestStage Quests.theLongDarkOfMoria
    a <- stage Enemies.forestSpider
    b <- stage Enemies.goblinSniper
    revealHunting
    beforeOne <- (.threat) <$> getPlayer one
    beforeTwo <- (.threat) <$> getPlayer two
    run $ CheckWindows [mkWhen RoundEnds]
    (.threat) <$> getPlayer one `shouldReturn` beforeOne
    (.threat) <$> getPlayer two `shouldReturn` (beforeTwo + 2)
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    huntingCards `shouldReturn` []
    run $ FocusArea one
    for_ [a, b] (\eid -> (.placement) <$> getEnemy eid `shouldReturn` StagingArea)

  it "Hunting the Ring triggers Isengard Archer's entry damage at the destination after the threat increase" . botfTwo $ \one two -> do
    heroOne <- beravorOf one
    heroTwo <- beravorOf two
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea two
    _ <- putQuestStage Quests.theLongDarkOfMoria
    eid <- stage Enemies.isengardArcher
    chooseTarget two heroTwo
    revealHunting
    beforeOne <- (.threat) <$> getPlayer one
    beforeTwo <- (.threat) <$> getPlayer two
    run $ CheckWindows [mkWhen RoundEnds]
    (.threat) <$> getPlayer two `shouldReturn` (beforeTwo + 1)
    chooseTarget one heroOne
    getDamage heroOne `shouldReturn` 2
    getDamage heroTwo `shouldReturn` 2
    (.threat) <$> getPlayer one `shouldReturn` beforeOne
    run $ FocusArea one
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
    huntingCards `shouldReturn` []

  it "multiple copies of Hunting the Ring resolve once each and are then discarded" . botf $ \pid -> do
    _ <- putQuestStage Quests.theLongDarkOfMoria
    _ <- stage Enemies.forestSpider
    revealHunting
    revealHunting
    beforeThreat <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen RoundEnds]
    resolveForcedChoices pid
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 2)
    huntingCards `shouldReturn` []

  it "Hunting the Ring is discarded with its quest and no longer triggers" . botf $ \pid -> do
    q <- putQuestStage Quests.theLongDarkOfMoria
    _ <- stage Enemies.forestSpider
    revealHunting
    case q of
      QuestTarget qid -> run $ LeaveQuestStage qid
      _ -> error "quest target expected"
    beforeThreat <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen RoundEnds]
    (.threat) <$> getPlayer pid `shouldReturn` beforeThreat
    huntingCards `shouldReturn` []

  it "Hunting the Ring uses the current first-player token holder when moving enemies" . botfTwo $ \one two -> do
    run $ SeparateStagingAreas [[one], [two]]
    run $ FocusArea one
    _ <- putQuestStage Quests.theLongDarkOfMoria
    eid <- stage Enemies.forestSpider
    revealHunting
    run $ SetFirstPlayer two
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    run $ FocusArea two
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
