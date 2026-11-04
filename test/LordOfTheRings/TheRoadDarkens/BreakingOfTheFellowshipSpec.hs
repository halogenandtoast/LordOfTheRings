module LordOfTheRings.TheRoadDarkens.BreakingOfTheFellowshipSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Card (CardDef (cdEncounterSet, cdStage), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyThreat, getStagingThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithId), charactersGuardedBy, getArchery, getArcheryTotal, getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (breakingOfTheFellowship, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), encounterDeckL, encounterDiscardL, questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkWhen)
import TestImport

botfTest :: (PlayerId -> GameT ()) -> IO ()
botfTest = gameTestIn breakingOfTheFellowship testDeck

botfTwo :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
botfTwo = twoPlayerTestIn breakingOfTheFellowship testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyPlacement :: EnemyId -> GameT Placement
enemyPlacement eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationAttrs
locationOf def = getsGame (toAttrs . fromJustNote "no such location" . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  (.id) <$> locationOf def

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

frodo :: GameT Hero
frodo = getHeroWithCode (toCardCode Heroes.frodoBagginsTheRoadDarkens)

useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use an ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

chooseFirstTarget :: HasCallStack => PlayerId -> GameT ()
chooseFirstTarget pid = chooseOptionMatching "choose a target" pid \case TargetLabel {} -> True; _ -> False

openLabel :: PlayerId -> GameT (Maybe Text)
openLabel pid = getQuestion pid <&> \case
  Just (QuestionLabel l _ _ _) -> Just l
  _ -> Nothing

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

-- | Advance from the stage in play to the stage given, its When Revealed resolved.
atStage' :: PlayerId -> CardDef -> GameT ()
atStage' pid def = do
  -- the players cannot advance from stage 1 while The Argonath is in play
  argonath <- getsGame (filter ((== toCardCode Locations.theArgonath) . toCardCode) . toList . (.entities.locations))
  for_ argonath \l -> run $ RemoveFromGame (toTarget l)
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL %~ dropWhile ((/= toCardCode def) . toCardCode))
  q <- getActiveQuest
  run $ CompleteQuest (toId q)
  resolveWhenRevealed pid

-- | The Ring-bearer of this box, if the game offers others.
chooseFrodo :: GameT ()
chooseFrodo = do
  lead <- getLead
  getQuestion lead >>= \case
    Just (QuestionLabel "$label.saga.chooseFellowshipHero" _ _ _) ->
      chooseOptionMatching "choose The Road Darkens' Frodo" lead \case
        TargetLabel _ (PutHeroIntoPlayAs _ _ card : _) -> toCardCode card == toCardCode Heroes.frodoBagginsTheRoadDarkens
        _ -> False
    _ -> pure ()

-- | Setup, the first quest flipped.
setUpOne :: PlayerId -> GameT ()
setUpOne pid = do
  run Setup
  chooseFrodo
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)

-- | The game set up for two, the first quest flipped.
setUpTwo :: GameT ()
setUpTwo = do
  run Setup
  chooseFrodo
  replicateM_ 2 do
    order <- getsGame (.playerOrder)
    asked <- filterM (fmap isJust . getQuestion) order
    for_ (take 1 asked) $ chooseLabel "$label.keepHand"
  lead <- getLead
  chooseOptionMatching "flip the quest" lead (const True)

-- | Each player chooses a stage 3 and the cards of stage 2's staging area, in turn.
divide :: PlayerId -> PlayerId -> GameT ()
divide one two = do
  run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
  useForcedAbilities one
  let answer = do
        asked <- filterM (fmap isJust . getQuestion) [one, two]
        for_ (take 1 asked) \p -> do
          chooseFirstTarget p
          answer
  answer

areaCount :: GameT Int
areaCount = getsGame (maybe 0 (length . separationAreas) . gameSeparation)

-- | The cards of the area not in focus.
heldEnemies :: GameT [CardCode]
heldEnemies = getsGame (concatMap (map toCardCode . toList . (.enemies)) . heldEntities)

spec :: Spec
spec = describe "Breaking of the Fellowship" do
  it "is a scenario the game can be dealt" . botfTest $ \_ ->
    isJust (lookupScenario "breaking-of-the-fellowship") `shouldBe` True


  describe "setup" do
    it "sets Frodo's Choice, Seat of Seeing and Parth Galen aside, The Argonath and Sarn Gebir staged" . botfTest $ \pid -> do
      setUpOne pid
      attrs <- scenarioAttrs
      sort (map toCardCode attrs.scenarioSetAside)
        `shouldBe` sort (map toCardCode [Objectives.frodosChoice, Locations.seatOfSeeing, Locations.parthGalen])
      all ((== Just BreakingOfTheFellowship) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      activeQuestCode `shouldReturn` toCardCode Quests.theGreatRiver
      (.placement) <$> locationOf Locations.theArgonath `shouldReturn` StagingArea
      (.placement) <$> locationOf Locations.sarnGebir `shouldReturn` StagingArea
      length attrs.questDeck `shouldBe` 6
      f <- frodo
      f.owner `shouldBe` pid

    it "builds the encounter deck" . botfTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.urukHaiCaptain, Enemies.urukHaiArcher, Enemies.urukHaiTracker, Enemies.urukHaiHunter, Locations.riverAnduin, Locations.woodedShoreline, Locations.slopesOfAmonHen, Treacheries.growingThreat, Treacheries.orcsOfTheWhiteHand, Treacheries.blackFeatheredArrows, Treacheries.fallenIntoEvil]
        `shouldBe` [2, 4, 3, 3, 4, 3, 3, 2, 2, 3, 1]

  describe "The Great River" do
    it "gives enemies +2 defense and bars engaging them, skips the encounter phase, and holds while The Argonath is in play" . botfTest $ \pid -> do
      setUpOne pid
      eid <- stage Enemies.urukHaiHunter
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` \ms -> DefenseModifier 2 `elem` ms && CannotEngagePlayer pid `elem` ms)
      getModifiers GameTarget >>= (`shouldSatisfy` elem (SkipPhase EncounterPhase))
      q <- getActiveQuest
      getModifiers (toTarget q) >>= (`shouldSatisfy` elem CannotBeDefeated)

    it "bars travel to The Argonath while Sarn Gebir is in play, its threat the players' count" . botfTest $ \pid -> do
      setUpOne pid
      argonath <- locationOf Locations.theArgonath
      getModifiers (LocationTarget argonath.id) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      gebir <- locationOf Locations.sarnGebir
      run $ RemoveFromGame (LocationTarget gebir.id)
      getModifiers (LocationTarget argonath.id) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      getStagingThreat `shouldReturn` 1

    it "skips the combat phase while The Argonath is active, and Sarn Gebir damages each exhausted character as it is explored" . botfTest $ \pid -> do
      setUpOne pid
      argonath <- locationOf Locations.theArgonath
      run $ PlaceLocation argonath.id ActiveLocation
      getModifiers GameTarget >>= (`shouldSatisfy` elem (SkipPhase CombatPhase))
      gebir <- locationOf Locations.sarnGebir
      beravor <- getHeroWithCode "01012"
      run $ Exhaust (toTarget beravor.id)
      run $ RemoveFromGame (LocationTarget argonath.id)
      run $ PlaceLocation gebir.id ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget gebir.id) Progress 4
      useForcedAbilities pid
      getCharacterDamage (toTarget beravor.id) `shouldReturn` 1

  describe "The Company Divided" do
    it "takes the Ring-bearer out of the game, sets engagement costs to 0, and skips the travel phase" . botfTest $ \pid -> do
      setUpOne pid
      eid <- stage Enemies.urukHaiHunter
      f0 <- frodo
      atStage' pid Quests.theCompanyDivided
      attrs <- scenarioAttrs
      let choice = fromJustNote "choice" $ find ((== toCardCode Objectives.frodosChoice) . toCardCode) attrs.scenarioSetAside
      charactersGuardedBy choice.id `shouldReturn` [toTarget f0.id]
      getEngagementCost eid `shouldReturn` 0
      getModifiers GameTarget >>= (`shouldSatisfy` elem (SkipPhase TravelPhase))

    it "has each player take his own staging area and stage 3, the cards of this one moved to them" . botfTwo $ \one two -> do
      setUpTwo
      lead <- getLead
      atStage' lead Quests.theCompanyDivided
      -- the cards of stage 2's staging area: The Argonath and Sarn Gebir, and an enemy
      _ <- stage Enemies.urukHaiHunter
      _ <- setEncounterDeck [Locations.riverAnduin, Locations.riverAnduin]
      divide one two
      areaCount `shouldReturn` 2
      -- no Company Divided left; each player's stage 3 is on its B side
      q <- getActiveQuest
      cdStage (toCardDef q) `shouldBe` Just 3
      (.flipped) (toAttrs q) `shouldBe` True
      attrs <- scenarioAttrs
      map toCardCode attrs.questDeck `shouldBe` [toCardCode Quests.theRingBearerSetsOut]
      -- Seat of Seeing and Frodo's Choice at the bottom of the encounter deck
      map toCardCode (drop (length attrs.encounterDeck - 2) attrs.encounterDeck) `shouldSatisfy` \cs -> sort cs == sort (map toCardCode [Locations.seatOfSeeing, Objectives.frodosChoice])

  describe "shared staging areas" do
    it "keeps each player's staging area his, the other player's in it held, and moves River Anduin to the left" . botfTwo $ \one two -> do
      setUpTwo
      run $ SeparateStagingAreas [[one], [two]]
      river <- stageLocation Locations.riverAnduin
      run $ FocusArea one
      getStagingThreat >>= (`shouldSatisfy` (>= 2))
      t1 <- threatOf one
      run $ CheckWindows [mkWhen $ PhaseEnds TravelPhase]
      useForcedAbilities one
      threatOf one `shouldReturn` (t1 + 1)
      getsGame (isJust . lookup river . (.entities.locations)) `shouldReturn` False
      run $ FocusArea two
      (.placement) <$> getLocation river `shouldReturn` StagingArea

    it "has each player engage only the enemies in his own staging area" . botfTwo $ \one two -> do
      setUpTwo
      -- past The Great River, whose enemies cannot be engaged
      woods <- genEncounterCard Quests.searchingTheWoods
      run $ ResetToStage woods []
      run $ SeparateStagingAreas [[one], [two]]
      run $ FocusArea two
      eid <- stage Enemies.urukHaiHunter
      run $ FocusArea one
      heldEnemies `shouldReturn` [toCardCode Enemies.urukHaiHunter]
      run $ RaiseThreat one 5
      run $ RaiseThreat two 5
      run $ EngagementCheck one 0
      run $ FocusArea two
      run $ EngagementCheck two 0
      enemyPlacement eid `shouldReturn` EngagedWith two

    it "has each player quest at his own stage in turn, against his own staging area" . botfTwo $ \one two -> do
      setUpTwo
      lead <- getLead
      let other = if lead == one then two else one
      run $ SeparateStagingAreas [[one], [two]]
      -- one River Anduin at each stage, each player's stage 3 his
      for_ [lead, other] \p -> do
        run $ FocusArea p
        woods <- genEncounterCard Quests.searchingTheWoods
        run $ ResetToStage woods []
      run $ FocusArea lead
      _ <- setEncounterDeck [Locations.riverAnduin, Locations.riverAnduin]
      t1 <- threatOf lead
      t2 <- threatOf other
      threat1 <- getStagingThreat
      run $ BeginPhase QuestPhase
      -- the first player commits, alone, and stages a card of his own
      getQuestion other `shouldReturn` Nothing
      chooseLabel "$label.doneCommitting" lead
      let passAll = do
            asked <- filterM (fmap isJust . getQuestion) [lead, other]
            for_ (take 1 asked) \p -> do
              before' <- getQuestion p
              passActions p
              after' <- getQuestion p
              when (fmap questionChoices before' /= fmap questionChoices after') passAll
      passAll
      threatOf lead `shouldReturn` (t1 + threat1 + 2)
      threatOf other `shouldReturn` t2
      -- then the other player, at his own stage
      chooseLabel "$label.doneCommitting" other
      passAll
      threatOf other `shouldReturn` (t2 + 2)
      threatOf lead `shouldReturn` (t1 + threat1 + 2)

    it "counts only the players at the stage for an Uruk-hai Archer's X" . botfTwo $ \one two -> do
      setUpTwo
      archer <- stage Enemies.urukHaiArcher
      getEnemyThreat archer `shouldReturn` 2
      run $ SeparateStagingAreas [[one], [two]]
      getEnemyThreat archer `shouldReturn` 1
      e <- getEnemy archer
      getArchery (EnemyTarget archer) (toCardDef e) `shouldReturn` 1

  describe "stage 3" do
    it "discards from the encounter deck in place of progress, revealing Frodo's Choice to advance to stage 4" . botfTest $ \pid -> do
      setUpOne pid
      f0 <- frodo
      atStage' pid Quests.theCompanyDivided
      attrs <- scenarioAttrs
      let choice = fromJustNote "choice" $ find ((== toCardCode Objectives.frodosChoice) . toCardCode) attrs.scenarioSetAside
      card <- genEncounterCard Quests.searchingTheWoods
      run $ ResetToStage card [card]
      run $ RemoveSetAsideFromGame [choice]
      four <- genEncounterCard Quests.theRingBearerSetsOut
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (\a -> a & questDeckL .~ [four] & encounterDeckL .~ [choice])
      run $ PlaceProgressOnQuest 1
      -- Frodo comes back to the player who revealed it, Frodo's Choice on him
      f <- frodo
      heroGuardedBy (toAttrs f) `shouldBe` Nothing
      f.owner `shouldBe` pid
      choices <- getsGame (filter ((== toCardCode Objectives.frodosChoice) . toCardCode) . toList . (.entities.objectives))
      map (.placement) choices `shouldBe` [AttachedTo (toTarget f0.id)]
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotLoseFirstPlayerToken)
      resolveWhenRevealed pid
      activeQuestCode `shouldReturn` toCardCode Quests.theRingBearerSetsOut
      (.placement) <$> locationOf Locations.parthGalen `shouldReturn` StagingArea

  describe "The Ring-bearer Sets Out" do
    it "reveals more in the quest phase, and is not won while Parth Galen is in play" . botfTest $ \pid -> do
      setUpOne pid
      atStage' pid Quests.theRingBearerSetsOut
      run $ BeginPhase QuestPhase
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))
      getModifiers GameTarget >>= (`shouldSatisfy` elem PlayersCannotWin)

  describe "the encounter cards" do
    it "turns a hero into an enemy in the first player's staging area with Fallen into Evil, until it is discarded" . botfTest $ \pid -> do
      beravor <- getHeroWithCode "01012"
      reveal [Treacheries.fallenIntoEvil]
      chooseTarget pid beravor.id
      let eid = EnemyId (unHeroId beravor.id)
      enemyPlacement eid `shouldReturn` StagingArea
      getEnemyThreat eid `shouldReturn` 2
      getEngagementCost eid `shouldReturn` 10
      fallen <- getsGame (fromJustNote "fallen" . find ((== toCardCode Treacheries.fallenIntoEvil) . toCardCode) . toList . (.entities.treacheries))
      run $ Discard (toTarget fallen)
      h <- getHeroWithCode "01012"
      h.owner `shouldBe` pid

    it "has Uruk-hai Captain bar allies with fewer hit points from defending" . botfTest $ \pid -> do
      captain <- stage Enemies.urukHaiCaptain
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      getModifiers ally >>= (`shouldSatisfy` elem (CannotDefendAgainst (EnemyWithId captain)))

    it "has Followed by Night put the topmost enemy of the discard pile into play engaged with you" . botfTest $ \pid -> do
      hunter <- genEncounterCard Enemies.urukHaiHunter
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [hunter])
      reveal [Treacheries.followedByNightTheRoadDarkens, Locations.riverAnduin]
      eid <- enemyOf Enemies.urukHaiHunter
      enemyPlacement eid `shouldReturn` EngagedWith pid

    it "raises your threat by 2 as a character you control is destroyed under Ill Fate" . botfTest $ \pid -> do
      reveal [Treacheries.illFate, Locations.riverAnduin]
      chooseFirstTarget pid
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      t <- threatOf pid
      run $ DealDamage GameSource ally 2
      useForcedAbilities pid
      threatOf pid `shouldReturn` (t + 2)

    it "adds Black Feathered Arrows' archery for each ally at this stage" . botfTest $ \pid -> do
      setUpOne pid
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      archery0 <- getArcheryTotal
      reveal [Treacheries.blackFeatheredArrows]
      getArcheryTotal `shouldReturn` (archery0 + 1)

    it "heals and strengthens each enemy at this stage with Orcs of the White Hand" . botfTest $ \_ -> do
      eid <- stage Enemies.urukHaiHunter
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 2
      reveal [Treacheries.orcsOfTheWhiteHand]
      (.damage) <$> getEnemy eid `shouldReturn` 0
      getEnemyThreat eid `shouldReturn` 3

    it "has Growing Threat gain doomed 2 and surge with no enemy engaged" . botfTest $ \pid -> do
      t <- threatOf pid
      reveal [Treacheries.growingThreat, Locations.riverAnduin]
      threatOf pid `shouldReturn` (t + 2)
      (.placement) <$> locationOf Locations.riverAnduin `shouldReturn` StagingArea

  describe "campaign mode" do
    it "has the player attach a Gift to a hero, earned, and shuffles in the burdens" . campaignTestIn breakingOfTheFellowship lotr testDeck $ \pid -> do
      run $ DoStep 9 Setup
      chooseFirstTarget pid
      chooseFirstTarget pid
      pool <- (.pool) <$> getCampaignLog
      let gifts = map toCardCode [Attachments.phialOfGaladriel, Attachments.threeGoldenHairs, Attachments.lorienRope, Attachments.leafWrappedLembas]
      map (.code) pool `shouldSatisfy` \cs -> length cs == 1 && all (`elem` gifts) cs
      attached <- getsGame (filter ((`elem` gifts) . toCardCode) . toList . (.entities.attachments))
      length attached `shouldBe` 1
      deck <- getEncounterDeck
      map (`copies` deck) [Treacheries.followedByNightTheRoadDarkens, Treacheries.illFate] `shouldBe` [1, 1]

    it "asks for the burden and the captive as the last stage is defeated, and records the fallen hero" . campaignTestIn breakingOfTheFellowship lotr testDeck $ \pid -> do
      beravor <- getHeroWithCode "01012"
      reveal [Treacheries.fallenIntoEvil]
      chooseTarget pid beravor.id
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (questDeckL .~ [])
      run AdvanceQuest
      openLabel pid `shouldReturn` Just "$label.breakingOfTheFellowship.chooseBurden"
      chooseFirstTarget pid
      openLabel pid `shouldReturn` Just "$label.breakingOfTheFellowship.chooseCaptive"
      chooseFirstTarget pid
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      map (.code) l.pool `shouldSatisfy` any (`elem` map toCardCode [Treacheries.followedByNightTheRoadDarkens, Treacheries.illFate])
      length (recorded Captives l) `shouldBe` 1
      l.fallenHeroes `shouldSatisfy` elem "01012"
