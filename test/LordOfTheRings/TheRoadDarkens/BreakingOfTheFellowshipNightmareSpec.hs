module LordOfTheRings.TheRoadDarkens.BreakingOfTheFellowshipNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy)
import LordOfTheRings.Scenario (breakingOfTheFellowship)
import TestImport

botf :: (PlayerId -> GameT ()) -> IO ()
botf = gameTestIn breakingOfTheFellowship testDeck

botfTwo :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
botfTwo = twoPlayerTestIn breakingOfTheFellowship testDeck

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
