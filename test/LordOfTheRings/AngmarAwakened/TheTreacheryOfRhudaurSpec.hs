module LordOfTheRings.AngmarAwakened.TheTreacheryOfRhudaurSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getAlly, getEnemy, getTimeCounters)
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Objective (Objective)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario (ScenarioAttrs (scenarioSetAside), lookupScenario, theTreacheryOfRhudaur)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

rhudaurTest :: (PlayerId -> GameT ()) -> IO ()
rhudaurTest = gameTestIn theTreacheryOfRhudaur testDeck

questOf :: CardDef -> GameT (Maybe Quest)
questOf def = getsGame (find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questTarget :: CardDef -> GameT Target
questTarget def = toTarget . fromJustNote "no such quest" <$> questOf def

progressOf :: CardDef -> GameT Int
progressOf def = maybe 0 (.progress) <$> questOf def

questPointsOf :: CardDef -> GameT Int
questPointsOf def = getQuestPoints . toAttrs . fromJustNote "no such quest" =<< questOf def

objectiveOf :: CardDef -> GameT (Maybe Objective)
objectiveOf def = getsGame (find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

amarthiul :: GameT AllyId
amarthiul = getsGame (fromJustNote "no Amarthiúl" . fmap toId . find ((== toCardCode Objectives.amarthiulTheTreacheryOfRhudaur) . toCardCode) . toList . (.entities.allies))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

sideQuests :: [CardDef]
sideQuests = [Quests.quietTheSpirits, Quests.siftThroughTheDebris, Quests.decipherAncientTexts]

-- | Defeat the side quest by placing its quest points of progress on it.
defeat :: CardDef -> GameT ()
defeat def = do
  q <- questTarget def
  n <- questPointsOf def
  run $ PlaceTokens GameSource q Progress n

spec :: Spec
spec = describe "The Treachery of Rhudaur" do
  it "is a scenario the game can be dealt" . rhudaurTest $ \_ ->
    isJust (lookupScenario "the-treachery-of-rhudaur") `shouldBe` True

  it "makes The Great Hall active, stages the side quests, sets Thaurdir aside and gives Amarthiúl to the first player" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    hall <- locationOf Locations.theGreatHall
    (.placement) <$> getLocation hall `shouldReturn` ActiveLocation
    for_ sideQuests \def -> isJust <$> questOf def `shouldReturn` True
    a <- amarthiul
    (.placement) <$> getAlly a `shouldReturn` InPlayArea pid
    setAside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    map toCardCode (setAside :: [EncounterCard]) `shouldSatisfy` elem (toCardCode Enemies.thaurdirTheTreacheryOfRhudaur)
    deck <- map toCardCode <$> getEncounterDeck
    for_ (Objectives.amarthiulTheTreacheryOfRhudaur : sideQuests) \c -> deck `shouldSatisfy` notElem (toCardCode c)
    (getTimeCounters =<< questTarget Quests.secretsOfRhudaur) `shouldReturn` 5

  it "flips a defeated side quest to its Clue, which a hero claims by exhausting" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    defeat Quests.siftThroughTheDebris
    isJust <$> questOf Quests.siftThroughTheDebris `shouldReturn` False
    victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
    map toCardCode victory `shouldSatisfy` notElem (toCardCode Quests.siftThroughTheDebris)
    heirloom <- fromJustNote "no Heirloom" <$> objectiveOf Objectives.heirloomOfIarchon
    heirloom.placement `shouldBe` StagingArea
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid heirloom
    beravor <- getHeroWithCode "01012"
    chooseTarget pid beravor
    (.placement) . fromJustNote "gone" <$> objectiveOf Objectives.heirloomOfIarchon `shouldReturn` AttachedTo (toTarget beravor)

  it "advances to Thaurdir's Pursuit once no side quest is left, which adds Thaurdir to the staging area" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    traverse_ defeat sideQuests
    isJust <$> questOf Quests.thaurdirsPursuit `shouldReturn` True
    resolveWhenRevealed' pid
    thaurdir <- enemyOf Enemies.thaurdirTheTreacheryOfRhudaur
    (.placement) <$> getEnemy thaurdir `shouldReturn` StagingArea

  it "removes the side quests and advances once its last time counter is gone" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    secrets <- questTarget Quests.secretsOfRhudaur
    run $ RemoveTokens GameSource secrets TimeCounter 5
    for_ sideQuests \def -> isJust <$> questOf def `shouldReturn` False
    isJust <$> questOf Quests.thaurdirsPursuit `shouldReturn` True

  it "takes 5 quest points off Thaurdir's Pursuit for each Clue held, and is not defeated while Thaurdir has hit points" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    defeat Quests.siftThroughTheDebris
    heirloom <- fromJustNote "no Heirloom" <$> objectiveOf Objectives.heirloomOfIarchon
    beravor <- getHeroWithCode "01012"
    run $ ClaimObjective (toId heirloom) (toTarget beravor)
    traverse_ defeat [Quests.quietTheSpirits, Quests.decipherAncientTexts]
    resolveWhenRevealed' pid
    questPointsOf Quests.thaurdirsPursuit `shouldReturn` 25
    pursuit <- questTarget Quests.thaurdirsPursuit
    run $ PlaceTokens GameSource pursuit Progress 25
    isJust <$> questOf Quests.thaurdirsPursuit `shouldReturn` True
    thaurdir <- enemyOf Enemies.thaurdirTheTreacheryOfRhudaur
    run $ DealDamage GameSource (toTarget thaurdir) 9
    isJust <$> questOf Quests.thaurdirsPursuit `shouldReturn` False

  it "gives the quest Haunting Fog is attached to 6 more quest points" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    sift <- questTarget Quests.siftThroughTheDebris
    run $ SetCurrentQuest sift
    _ <- setEncounterDeck [Treacheries.hauntingFog]
    run RevealEncounterCard
    resolveRevealed
    chooseOptionMatching "exhaust a character" pid (const True)
    questPointsOf Quests.siftThroughTheDebris `shouldReturn` 24

  it "gives the side quests 2 more quest points while Decrepit Remains is staged" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    staged Locations.decrepitRemains
    questPointsOf Quests.quietTheSpirits `shouldReturn` 18

  it "puts the first 2 progress each round on Forbidden Descent" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    staged Locations.forbiddenDescent
    descent <- locationOf Locations.forbiddenDescent
    sift <- questTarget Quests.siftThroughTheDebris
    run $ PlaceTokens GameSource sift Progress 3
    (.progress) <$> getLocation descent `shouldReturn` 2
    progressOf Quests.siftThroughTheDebris `shouldReturn` 1
    run $ PlaceTokens GameSource sift Progress 3
    (.progress) <$> getLocation descent `shouldReturn` 2
    progressOf Quests.siftThroughTheDebris `shouldReturn` 4

  it "raises each player's threat by 2 as Forbidden Descent leaves the staging area" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    staged Locations.forbiddenDescent
    descent <- locationOf Locations.forbiddenDescent
    threat <- threatOf pid
    run $ Discard (toTarget descent)
    threatOf pid `shouldReturn` threat + 2

  it "has Traitorous Wight remove 2 progress from each quest card as it engages" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    for_ [Quests.quietTheSpirits, Quests.siftThroughTheDebris] \def -> do
      q <- questTarget def
      run $ PlaceTokens GameSource q Progress 3
    staged Enemies.traitorousWight
    wight <- enemyOf Enemies.traitorousWight
    run $ EngageEnemy pid wight
    progressOf Quests.quietTheSpirits `shouldReturn` 1
    progressOf Quests.siftThroughTheDebris `shouldReturn` 1

  it "has Wight of Rhudaur raise each threat by 2 when revealed with no progress on the current quest" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    threat <- threatOf pid
    _ <- setEncounterDeck [Enemies.wightOfRhudaur]
    run RevealEncounterCard
    resolveRevealed
    threatOf pid `shouldReturn` threat + 2

  it "returns the topmost Undead enemy to the staging area as the encounter deck resets under Ghostly Ruins" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    staged Locations.ghostlyRuins
    wight <- genEncounterCard Enemies.wightOfRhudaur
    _ <- setEncounterDeck []
    run $ AddToEncounterDiscard [wight]
    putGame . (phaseL .~ QuestPhase) =<< getGame
    run $ DiscardTopOfEncounterDeck GameSource GameTarget 0
    _ <- enemyOf Enemies.wightOfRhudaur
    map toCardCode <$> getEncounterDiscard `shouldReturn` []

  it "loses the game if Amarthiúl leaves play" . rhudaurTest $ \pid -> do
    flipFirstQuest pid
    a <- amarthiul
    run $ Discard (toTarget a)
    getsGame (.outcome) `shouldReturn` Just Lost
 where
  -- the stage, not a side quest, is the one waiting for its When Revealed
  -- then the player's search for an Undead enemy, which finds what it finds
  resolveWhenRevealed' pid = do
    chooseTarget pid =<< questTarget Quests.thaurdirsPursuit
    getQuestion pid >>= \case
      Just _ -> chooseOptionMatching "search" pid (const True)
      Nothing -> pure ()
