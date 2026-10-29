module LordOfTheRings.VoiceOfIsengard.IntoFangornSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getTimeCounters)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Scenario (intoFangorn, lookupScenario, questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

fangornTest :: (PlayerId -> GameT ()) -> IO ()
fangornTest = gameTestIn intoFangorn testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [o.placement | o <- toList g.entities.objectives, toCardCode o == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  run . AddToStaging =<< genEncounterCard def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

mugash :: GameT ObjectiveId
mugash = getsGame (fromJustNote "no Mugash" . fmap toId . find ((== toCardCode Objectives.mugashObjective) . toCardCode) . toList . (.entities.objectives))

-- | Mugash in the staging area, free, then attached to Théodred.
mugashOnTheodred :: GameT Target
mugashOnTheodred = do
  run . AddToStaging =<< genEncounterCard Objectives.mugashObjective
  theodred <- toTarget <$> getHeroWithCode "01002"
  oid <- mugash
  run $ ClaimObjective oid theodred
  pure theodred

withQuestDeck :: [CardDef] -> GameT ()
withQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

activeStage :: GameT CardCode
activeStage = toCardCode <$> getActiveQuest

spec :: Spec
spec = describe "Into Fangorn" do
  it "is a scenario the game can be dealt" . fangornTest $ \_ ->
    isJust (lookupScenario "into-fangorn") `shouldBe` True

  it "stages Edge of Fangorn with Mugash attached and gives the stage 4 time counters" . fangornTest $ \pid -> do
    flipFirstQuest pid
    placementOf Locations.edgeOfFangorn `shouldReturn` Just StagingArea
    edge <- getsGame (fromJustNote "no edge" . fmap toId . headMay . filter ((== toCardCode Locations.edgeOfFangorn) . toCardCode) . toList . (.entities.locations))
    placementOf Objectives.mugashObjective `shouldReturn` Just (AttachedTo (LocationTarget edge))
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Objectives.mugashObjective)
    q <- getActiveQuest
    getTimeCounters (toTarget q) `shouldReturn` 4

  it "frees Mugash to the staging area as Edge of Fangorn is explored" . fangornTest $ \pid -> do
    flipFirstQuest pid
    edge <- getsGame (fromJustNote "no edge" . fmap toId . headMay . toList . (.entities.locations))
    run $ AddToVictoryDisplay (toTarget edge)
    placementOf Objectives.mugashObjective `shouldReturn` Just StagingArea

  it "lets a player exhaust a hero to claim a free Mugash, and returns him to the encounter deck as that hero takes damage" . fangornTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.mugashObjective
    oid <- mugash
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid oid
    theodred <- getHeroWithCode "01002"
    chooseTarget pid theodred
    placementOf Objectives.mugashObjective `shouldReturn` Just (AttachedTo (toTarget theodred))
    heroExhausted . toAttrs <$> getHeroWithCode "01002" `shouldReturn` True
    chooseLabel "$label.donePlanning" pid
    run $ DealDamage GameSource (toTarget theodred) 1
    map toCardCode . take 1 <$> getEncounterDeck `shouldReturn` [toCardCode Objectives.mugashObjective]

  it "does not advance Into the Woods without Mugash on a hero, then does once he is" . fangornTest $ \pid -> do
    q <- putQuestStage Quests.intoTheWoods
    withQuestDeck [Quests.escapeFromFangorn]
    run $ PlaceTokens GameSource q Progress 9
    activeStage `shouldReturn` toCardCode Quests.intoTheWoods
    _ <- mugashOnTheodred
    resolveWhenRevealed pid
    activeStage `shouldReturn` toCardCode Quests.escapeFromFangorn

  it "shuffles Mugash into the encounter deck and moves to The Angry Forest as Into the Woods runs out of time" . fangornTest $ \_ -> do
    q <- putQuestStage Quests.intoTheWoods
    run $ Flip q
    _ <- mugashOnTheodred
    run $ RemoveTokens GameSource q TimeCounter 4
    activeStage `shouldReturn` toCardCode Quests.theAngryForest
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Objectives.mugashObjective]
    toTarget <$> getActiveQuest >>= getTimeCounters >>= (`shouldBe` 3)

  it "goes back to Escape from Fangorn from The Angry Forest on 6 progress with Mugash on a hero" . fangornTest $ \pid -> do
    q <- putQuestStage Quests.theAngryForest
    run $ PlaceTokens GameSource q Progress 6
    activeStage `shouldReturn` toCardCode Quests.theAngryForest
    _ <- mugashOnTheodred
    q' <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q' Progress 1
    activeStage `shouldReturn` toCardCode Quests.escapeFromFangorn
    resolveWhenRevealed pid
    chooseLabel "$label.continue" pid
    -- defeated with Mugash on a hero, it wins the game
    q2 <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q2 Progress 12
    getsGame (.outcome) `shouldReturn` Just Won

  it "reveals an enemy from the discard pile and resets its time as The Angry Forest runs out of time" . fangornTest $ \pid -> do
    q <- putQuestStage Quests.theAngryForest
    run $ Flip q
    _ <- setEncounterDeck [Treacheries.theForestsMalice, Enemies.darkHeartedHuorn, Treacheries.theForestsMalice]
    run $ RemoveTokens GameSource q TimeCounter 3
    getEncounterDiscard >>= (`shouldSatisfy` ((== 3) . length))
    huorn <- headMay . filter ((== toCardCode Enemies.darkHeartedHuorn) . toCardCode) <$> getEncounterDiscard
    chooseTarget pid (fromJustNote "no huorn" huorn)
    placementOf Enemies.darkHeartedHuorn `shouldReturn` Just StagingArea
    getTimeCounters q `shouldReturn` 3

  it "keeps an engaged Huorn from attacking, removing progress from the quest as combat begins" . fangornTest $ \pid -> do
    q <- putQuestStage Quests.escapeFromFangorn
    run $ PlaceTokens GameSource q Progress 2
    eid <- engaged pid Enemies.darkHeartedHuorn
    _ <- setEncounterDeck [Treacheries.theForestsMalice]
    run $ BeginPhase CombatPhase
    (.progress) <$> getActiveQuest `shouldReturn` 1
    length . (.shadowCards) <$> getEnemy eid `shouldReturn` 0
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` maybe True (\case QuestionLabel l _ _ _ -> not ("$label.declareDefender" `isPrefixOf` l); _ -> True))

  it "removes the Huorn's progress from the active location once the quest has none" . fangornTest $ \pid -> do
    _ <- putQuestStage Quests.escapeFromFangorn
    run . AddToStaging =<< genEncounterCard Locations.heartOfFangorn
    heart <- getsGame (fromJustNote "no heart" . fmap toId . headMay . toList . (.entities.locations))
    run $ PlaceLocation heart ActiveLocation
    run $ PlaceTokens GameSource (toTarget heart) Progress 2
    _ <- engaged pid Enemies.deadlyHuorn
    run $ BeginPhase CombatPhase
    (.progress) <$> getLocation heart `shouldReturn` 1

  it "raises the engaged player's threat by 2 with Dark-Hearted Huorn as the resource phase begins" . fangornTest $ \pid -> do
    _ <- engaged pid Enemies.darkHeartedHuorn
    threat <- (.threat) <$> getPlayer pid
    run $ BeginPhase ResourcePhase
    (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  it "lets each player refresh no more than 5 characters while Heart of Fangorn is staged" . fangornTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.heartOfFangorn
    allies <- replicateM 3 (putAllyIntoPlay pid Allies.guardOfTheCitadel)
    heroes <- map toTarget . toList <$> getsGame (.entities.heroes)
    for_ (heroes <> allies) \c -> run $ Exhaust c
    run $ BeginPhase RefreshPhase
    for_ (take 5 $ heroes <> allies) (chooseTarget pid)
    exhausted <- getsGame \g -> length [a | a <- toList g.entities.allies, allyExhausted (toAttrs a)]
    exhausted `shouldBe` 1

  it "puts a Huorn into play engaged with each player when none is engaged, with The Forest's Malice" . fangornTest $ \pid -> do
    _ <- setEncounterDeck [Treacheries.theForestsMalice, Enemies.angryHuorn]
    run RevealEncounterCard
    resolveRevealed
    huorn <- headMay . filter ((== toCardCode Enemies.angryHuorn) . toCardCode) <$> getEncounterDeck
    chooseTarget pid (fromJustNote "no huorn" huorn)
    placementOf Enemies.angryHuorn `shouldReturn` Just (EngagedWith pid)

  it "has engaged Huorns attack instead, with The Forest's Malice" . fangornTest $ \pid -> do
    _ <- engaged pid Enemies.angryHuorn
    _ <- setEncounterDeck [Treacheries.theForestsMalice, Enemies.deadlyHuorn]
    run RevealEncounterCard
    resolveRevealed
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` maybe False (\case QuestionLabel l _ _ _ -> "$label.declareDefender" `isPrefixOf` l; _ -> False))
    placementOf Enemies.deadlyHuorn `shouldReturn` Nothing
