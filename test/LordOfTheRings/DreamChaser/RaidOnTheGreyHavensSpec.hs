module LordOfTheRings.DreamChaser.RaidOnTheGreyHavensSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Modifier (ModifierType (CannotTakeDamage))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, raidOnTheGreyHavens)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), setAsideL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Matcher (getAflameLocations)
import LordOfTheRings.Window (WindowType (EnemyAttacking, PhaseEnds), mkAfter, mkWhen)
import TestImport

raidTest :: (PlayerId -> GameT ()) -> IO ()
raidTest = gameTestIn raidOnTheGreyHavens testDeck

-- | The card added to the staging area, and its id among the locations.
stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such location" . fmap toId . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies))

havensBurn :: GameT Target
havensBurn = do
  run . AddToStaging =<< genEncounterCard Objectives.theHavensBurn
  getsGame (toTarget . fromJustNote "no Havens Burn" . headMay . toList . (.entities.objectives))

underneath :: GameT [CardCode]
underneath = getsGame (concatMap (map toCardCode . objectiveUnderneath . toAttrs) . toList . (.entities.objectives))

damageOn :: LocationId -> GameT Int
damageOn = getDamage . LocationTarget

resourcesOn :: EnemyId -> GameT Int
resourcesOn eid = getsGame (maybe 0 (countTokens Resource . enemyTokens . toAttrs) . find ((== eid) . toId) . toList . (.entities.enemies))

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | Sahír's Advance in play on its B side, its When Revealed resolved.
advancing :: PlayerId -> GameT Target
advancing pid = do
  q <- putQuestStage Quests.sahirsAdvance
  run $ Flip q
  resolveWhenRevealed pid
  pure q

spec :: Spec
spec = describe "Raid on the Grey Havens" do
  it "is a scenario the game can be dealt" . raidTest $ \_ ->
    isJust (lookupScenario "raid-on-the-grey-havens") `shouldBe` True

  it "sets the Stormcaller Elite aside and adds The Havens Burn, the Dream-chaser, a Ravager and an Aflame location to the staging area" . raidTest $ \pid -> do
    flipFirstQuest pid
    getsGame (sort . map toCardCode . (.scenarioSetAside) . toAttrs . (.scenario))
      `shouldReturn` sort (map toCardCode [Enemies.captainSahir, Enemies.naasiyah])
    getsGame (map toCardCode . toList . (.entities.objectives)) `shouldReturn` [toCardCode Objectives.theHavensBurn]
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.sahirsRavager]
    ls <- getsGame (map toAttrs . toList . (.entities.locations))
    map (.placement) ls `shouldBe` [StagingArea, StagingArea]
    sort (map toCardCode ls) `shouldSatisfy` elem (toCardCode Locations.dreamChaserLocation)
    burning <- getAflameLocations
    length burning `shouldBe` 2
    getEncounterDiscard `shouldReturn` []

  -- \* Aflame

  it "places 1 damage on each Aflame location in the staging area at the end of the round" . raidTest $ \_ -> do
    piers <- stagedLocation Locations.burningPiers
    harbor <- stagedLocation Locations.mithlondHarbor
    run EndRound
    damageOn piers `shouldReturn` 1
    damageOn harbor `shouldReturn` 0
    -- Burning Piers gets +1 threat for each damage on it; the harbor has 1
    getStagingThreat `shouldReturn` 3

  it "destroys an Aflame location with as much damage as its Aflame value, placing it underneath The Havens Burn, which reveals an encounter card" . raidTest $ \_ -> do
    _ <- havensBurn
    dreamChaser <- stagedLocation Locations.dreamChaserLocation
    piers <- stagedLocation Locations.burningPiers
    _ <- setEncounterDeck [Locations.mithlondHarbor, Locations.mithlondHarbor]
    run $ DealDamage GameSource (LocationTarget piers) 4
    underneath `shouldReturn` []
    run $ DealDamage GameSource (LocationTarget piers) 1
    underneath `shouldReturn` [toCardCode Locations.burningPiers]
    -- Burning Piers deals 3 damage to the Dream-chaser as it goes
    damageOn dreamChaser `shouldReturn` 3
    -- the harbor revealed surges into the other
    length <$> getEncounterDeck `shouldReturn` 0
    outcome `shouldReturn` Nothing

  it "loses the game once 3 more cards than players are underneath The Havens Burn" . raidTest $ \_ -> do
    burn <- havensBurn
    _ <- setEncounterDeck []
    cards <- traverse (fmap EncounterCard . genEncounterCard) [Locations.whiteShip, Locations.whiteShip, Locations.elvenCaravel]
    run $ PlaceUnderneath burn cards
    outcome `shouldReturn` Nothing
    wave <- stagedLocation Locations.elvenWaveRunner
    run $ DealDamage GameSource (LocationTarget wave) 3
    outcome `shouldReturn` Just Lost

  it "loses the game as the Dream-chaser is placed underneath The Havens Burn" . raidTest $ \_ -> do
    _ <- havensBurn
    dreamChaser <- stagedLocation Locations.dreamChaserLocation
    run $ DealDamage GameSource (LocationTarget dreamChaser) 12
    outcome `shouldReturn` Just Lost

  it "deals The Fires Spread's damage to each Aflame location, surging with less than 3 dealt" . raidTest $ \_ -> do
    piers <- stagedLocation Locations.burningPiers
    _ <- setEncounterDeck [Treacheries.theFiresSpread, Locations.mithlondHarbor]
    run RevealEncounterCard
    resolveRevealed
    damageOn piers `shouldReturn` 1
    getEncounterDeck `shouldReturn` []

  it "counts only the damage The Fires Spread deals, surging when a location takes none" . raidTest $ \_ -> do
    replicateM_ 3 $ run . AddToStaging =<< genEncounterCard Locations.burningPiers
    piers <- map fst <$> getAflameLocations
    length piers `shouldBe` 3
    for_ (take 1 piers) \l -> run $ CreateEffect $ Effect GameSource (LocationTarget l) [CannotTakeDamage] UntilEndOfPhase
    _ <- setEncounterDeck [Treacheries.theFiresSpread, Locations.mithlondHarbor]
    run RevealEncounterCard
    resolveRevealed
    getEncounterDeck `shouldReturn` []

  -- \* The Corsair's Assault

  it "deals damage to the lowest Aflame location for each Corsair enemy in play at the end of the combat phase" . raidTest $ \pid -> do
    q <- putQuestStage Quests.theCorsairsAssault
    run $ Flip q
    passActions pid
    piers <- stagedLocation Locations.burningPiers
    wave <- stagedLocation Locations.elvenWaveRunner
    _ <- stagedEnemy Enemies.corsairArsonist
    _ <- stagedEnemy Enemies.umbarRaider
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    damageOn wave `shouldReturn` 2
    damageOn piers `shouldReturn` 0

  it "makes Sahír's Ravager place a resource on itself after it attacks, then burn the lowest Aflame location as much" . raidTest $ \pid -> do
    wave <- stagedLocation Locations.elvenWaveRunner
    eid <- stagedEnemy Enemies.sahirsRavager
    run $ PlaceTokens GameSource (EnemyTarget eid) Resource 1
    run $ EngageEnemy pid eid
    run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    resourcesOn eid `shouldReturn` 2
    damageOn wave `shouldReturn` 2

  -- \* Sahír's Advance

  it "adds the Stormcaller Elite with resources for the Dream-chaser's damage, and 1 on each other Raider" . raidTest $ \pid -> do
    elite <- traverse genEncounterCard [Enemies.captainSahir, Enemies.naasiyah]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ elite)
    dreamChaser <- stagedLocation Locations.dreamChaserLocation
    run $ DealDamage GameSource (LocationTarget dreamChaser) 2
    raider <- stagedEnemy Enemies.umbarRaider
    _ <- setEncounterDeck [Enemies.umbarRaider]
    _ <- advancing pid
    enemies <- getsGame (map toAttrs . toList . (.entities.enemies))
    let on def = [countTokens Resource (enemyTokens e) | e <- enemies, toCardCode e == toCardCode def]
    on Enemies.captainSahir `shouldBe` [2]
    on Enemies.naasiyah `shouldBe` [2]
    resourcesOn raider `shouldReturn` 1
    -- the one revealed enters play with 1, then gets 1 more
    sort (on Enemies.umbarRaider) `shouldBe` [1, 2]

  it "removes damage from locations in play in place of progress on the quest" . raidTest $ \pid -> do
    q <- advancing pid
    piers <- stagedLocation Locations.burningPiers
    run $ DealDamage GameSource (LocationTarget piers) 3
    run $ PlaceTokens GameSource q Progress 2
    damageOn piers `shouldReturn` 1
    getsGame (maybe 0 (.progress) . headMay . toList . (.entities.quests)) `shouldReturn` 0

  it "wins once Captain Sahír is in the victory display and no location in play has damage" . raidTest $ \pid -> do
    q <- advancing pid
    piers <- stagedLocation Locations.burningPiers
    run $ DealDamage GameSource (LocationTarget piers) 1
    sahir <- genEncounterCard Enemies.captainSahir
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (victoryDisplayL %~ (EncounterCard sahir :))
    run . CheckStage =<< getsGame (fromJustNote "no quest" . fmap toId . headMay . toList . (.entities.quests))
    outcome `shouldReturn` Nothing
    run $ PlaceTokens GameSource q Progress 1
    outcome `shouldReturn` Just Won
