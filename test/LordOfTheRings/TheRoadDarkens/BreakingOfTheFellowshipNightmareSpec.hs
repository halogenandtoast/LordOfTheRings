module LordOfTheRings.TheRoadDarkens.BreakingOfTheFellowshipNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy, getMainQuestProgress)
import LordOfTheRings.Scenario (breakingOfTheFellowship)
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Modifier (ModifierType (Blank, GainKeyword, CannotEngagePlayer, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy))
import LordOfTheRings.Keyword (Keyword (Toughness))
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
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

spec :: Spec
spec = describe "Breaking of the Fellowship Nightmare" do
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
