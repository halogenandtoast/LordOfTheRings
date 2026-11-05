module LordOfTheRings.TreasonOfSaruman.TheRoadToIsengardSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (FirstPlayer))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theRoadToIsengard)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (questProgressCanceled, questProgressThisPhase)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

isengardTest :: (PlayerId -> GameT ()) -> IO ()
isengardTest = gameTestIn theRoadToIsengard testDeck

-- | The Lord of the Rings campaign, Helm's Deep just won, the test deck's heroes recorded.
lotr :: CampaignLog
lotr =
  (newCampaignLog "the-lord-of-the-rings")
    { campaignLogHeroes = [["01012", "01007", "01002"]]
    , campaignLogCompleted =
        [ CompletedScenario sid 100
        | sid <-
            [ "a-shadow-of-the-past"
            , "a-knife-in-the-dark"
            , "flight-to-the-ford"
            , "the-ring-goes-south"
            , "journey-in-the-dark"
            , "breaking-of-the-fellowship"
            , "the-uruk-hai"
            , "helms-deep"
            ]
        ]
    }

campaignTest :: (PlayerId -> GameT ()) -> IO ()
campaignTest = campaignTestIn theRoadToIsengard lotr testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOfLocation :: LocationId -> GameT Placement
placementOfLocation lid = (.placement) <$> getLocation lid

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

objectivesOf :: CardDef -> GameT [ObjectiveAttrs]
objectivesOf def = getsGame (map toAttrs . filter ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

alliesOf :: CardDef -> GameT [AllyAttrs]
alliesOf def = getsGame (map toAttrs . filter ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

questionKey :: PlayerId -> GameT (Maybe Text)
questionKey pid = fmap (\case QuestionLabel l _ _ _ -> l; _ -> "") <$> getQuestion pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | Pass each player's open action window, in turn.
passAll :: [PlayerId] -> GameT ()
passAll = traverse_ passActions

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

-- | Setup clicked through: the Ents staged, stage 1B in play.
setUp :: PlayerId -> GameT ()
setUp = flipFirstQuest

-- | Stage 1's Forced effect, the players advancing to stage 2 with the given cards to reveal,
-- and stage 2A's When Revealed resolved.
toStage2 :: PlayerId -> [CardDef] -> GameT ()
toStage2 pid revealed = do
  _ <- setEncounterDeck revealed
  run $ CheckWindows [mkAfter RoundEnds]
  chooseLabel "$label.rouseTheEnts.advanceToStage2" pid
  resolveRevealed

-- | Stage 2 left as its fifth damage is placed, stage 3A's When Revealed resolved.
toStage3 :: PlayerId -> GameT ()
toStage3 pid = do
  toStage2 pid [Locations.ringWallOfIsengard]
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Damage 4
  ringWall <- locationOf Locations.ringWallOfIsengard
  run $ PlaceTokens GameSource (LocationTarget ringWall) Progress 2
  useForcedAbilities pid
  resolveWhenRevealed pid

spec :: Spec
spec = describe "The Road to Isengard" do
  it "is a scenario the game can be dealt" . isengardTest $ \_ ->
    isJust (lookupScenario "the-road-to-isengard") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "builds the encounter deck from The Road to Isengard and Snaga Orcs, the stage 1A cards set aside" . isengardTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 32
      all ((`elem` map Just [TheRoadToIsengard, SnagaOrcs]) . cdEncounterSet . toCardDef) deck `shouldBe` True
      map (`copies` deck) [Objectives.entOfFangorn, Enemies.sarumanTheRoadToIsengard, Locations.orthanc, Locations.gatesOfIsengard, Treacheries.sarumansVoice, Enemies.snaga, Locations.steamingVent]
        `shouldBe` [0, 0, 0, 0, 0, 3, 3]
      aside <- scenarioSetAside <$> scenarioAttrs
      map (`copies` aside) [Objectives.entOfFangorn, Enemies.sarumanTheRoadToIsengard, Objectives.sarumansStaffTheRoadToIsengard, Locations.orthanc, Locations.gatesOfIsengard]
        `shouldBe` [4, 1, 1, 1, 1]
      map toCardCode . scenarioQuestDeck <$> scenarioAttrs `shouldReturn` map toCardCode [Quests.breakingIsengard, Quests.theVoiceOfSaruman]

    it "adds each Ent of Fangorn to the staging area, the first player taking the Fellowship Aragorn" . isengardTest $ \pid -> do
      setUp pid
      ents <- objectivesOf Objectives.entOfFangorn
      map (.objectivePlacement) ents `shouldBe` replicate 4 StagingArea
      aragorn <- getHeroWithCode "143001"
      aragorn.owner `shouldBe` pid
      activeQuestCode `shouldReturn` toCardCode Quests.rouseTheEnts
      aside <- scenarioSetAside <$> scenarioAttrs
      map (`copies` aside) [Objectives.entOfFangorn, Enemies.sarumanTheRoadToIsengard] `shouldBe` [0, 1]

    it "shuffles Saruman's Voice into the encounter deck in campaign mode" . campaignTest $ \pid -> do
      setUp pid
      deck <- getEncounterDeck
      copies Treacheries.sarumansVoice deck `shouldBe` 1
      length deck `shouldBe` 33

  -- \* The stages

  describe "Rouse the Ents" do
    it "keeps the players from drawing, gaining resources, committing and staging" . isengardTest $ \pid -> do
      setUp pid
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` \ms -> CannotDrawCards `elem` ms && CannotCommitCharacters `elem` ms)
      beravor <- hero "01012"
      getModifiers beravor >>= (`shouldSatisfy` \ms -> CannotCollectResources `elem` ms && CannotGainResourcesFromCardEffects `elem` ms)
      getModifiers GameTarget >>= (`shouldSatisfy` elem SkipStagingStep)
      hand <- handSize pid
      run $ DrawCards (Drawing pid 1)
      handSize pid `shouldReturn` hand

    it "places a resource at the end of the round, letting the players stay" . isengardTest $ \pid -> do
      setUp pid
      run $ CheckWindows [mkAfter RoundEnds]
      chooseLabel "$label.rouseTheEnts.stay" pid
      q <- getActiveQuest
      toCardCode q `shouldBe` toCardCode Quests.rouseTheEnts
      countTokens Resource (questTokens $ toAttrs q) `shouldBe` 1

    it "reveals 1 card for each resource, removes the Ents left in the staging area and advances" . isengardTest $ \pid -> do
      setUp pid
      run $ CheckWindows [mkAfter RoundEnds]
      chooseLabel "$label.rouseTheEnts.stay" pid
      toStage2 pid [Locations.openPit, Locations.ringWallOfIsengard, Locations.steamingVent]
      activeQuestCode `shouldReturn` toCardCode Quests.breakingIsengard
      objectivesOf Objectives.entOfFangorn `shouldReturn` []
      -- the discard pile shuffled in, 2 of the 3 cards are revealed
      length <$> getEncounterDeck `shouldReturn` 1
      staged <- getsGame (filter (\l -> l.placement == StagingArea && toCardCode l `elem` map toCardCode [Locations.openPit, Locations.ringWallOfIsengard, Locations.steamingVent]) . toList . (.entities.locations))
      length staged `shouldBe` 2

  describe "Ent of Fangorn" do
    it "takes a resource for a hero with more willpower than the discarded card's threat, and joins the player at 3" . isengardTest $ \pid -> do
      setUp pid
      inPhase QuestPhase
      ent <- fromJustNote "no Ent" . headMay <$> objectivesOf Objectives.entOfFangorn
      -- Eowyn, Beravor and Aragorn each have more willpower than Isengard Orc's 1 threat
      for_ (zip [1 :: Int, 2, 3] ["01007", "01012", "143001"]) \(n, code) -> do
        _ <- setEncounterDeck [Enemies.isengardOrc]
        run $ ActionWindow pid
        chooseAbilityOf pid (ObjectiveTarget ent.objectiveId)
        chooseTarget pid =<< hero code
        when (n < 3) $ ((.resources) <$> (fromJustNote "no Ent" . find ((== ent.objectiveId) . (.objectiveId)) <$> objectivesOf Objectives.entOfFangorn)) `shouldReturn` n
      allies <- alliesOf Objectives.entOfFangorn
      map (.placement) allies `shouldBe` [InPlayArea pid]
      length <$> objectivesOf Objectives.entOfFangorn `shouldReturn` 3

    it "takes no resource when the hero's willpower is not greater" . isengardTest $ \pid -> do
      setUp pid
      inPhase QuestPhase
      ent <- fromJustNote "no Ent" . headMay <$> objectivesOf Objectives.entOfFangorn
      -- Théodred's willpower is 1; Saruman's Orders has no threat but Lieutenant of Orthanc 3
      _ <- setEncounterDeck [Enemies.lieutenantOfOrthanc]
      run $ ActionWindow pid
      chooseAbilityOf pid (ObjectiveTarget ent.objectiveId)
      chooseTarget pid =<< hero "01002"
      ((.resources) . fromJustNote "no Ent" . find ((== ent.objectiveId) . (.objectiveId))) <$> objectivesOf Objectives.entOfFangorn `shouldReturn` 0

  describe "Breaking Isengard" do
    it "makes Gates of Isengard active, stages Saruman and Orthanc, and keeps the players from traveling to Orthanc" . isengardTest $ \pid -> do
      setUp pid
      toStage2 pid [Locations.openPit]
      (placementOfLocation =<< locationOf Locations.gatesOfIsengard) `shouldReturn` ActiveLocation
      saruman <- enemyOf Enemies.sarumanTheRoadToIsengard
      placementOfEnemy saruman `shouldReturn` StagingArea
      orthanc <- locationOf Locations.orthanc
      placementOfLocation orthanc `shouldReturn` StagingArea
      getModifiers (LocationTarget orthanc) >>= (`shouldSatisfy` \ms -> CannotBeTraveledTo `elem` ms && ImmuneToPlayerCardEffects `elem` ms)
      getModifiers (EnemyTarget saruman) >>= (`shouldSatisfy` \ms -> CannotLeaveStagingArea `elem` ms && ImmuneToPlayerCardEffects `elem` ms)

    it "takes 1 damage as a non-unique location is explored, advancing at 5" . isengardTest $ \pid -> do
      setUp pid
      toStage2 pid [Locations.openPit]
      pit <- locationOf Locations.openPit
      run $ PlaceTokens GameSource (LocationTarget pit) Progress 3
      useForcedAbilities pid
      q <- getActiveQuest
      countTokens Damage (questTokens $ toAttrs q) `shouldBe` 1
      run $ PlaceTokens GameSource (toTarget q) Damage 3
      _ <- setEncounterDeck []
      eastemnet <- stageLocation Locations.eastemnet
      run $ PlaceTokens GameSource (LocationTarget eastemnet) Progress 3
      useForcedAbilities pid
      activeQuestCode `shouldReturn` toCardCode Quests.theVoiceOfSaruman

    it "lets a player exhaust his Ent of Fangorn to place 2 progress on a non-unique location" . isengardTest $ \pid -> do
      setUp pid
      ent <- fromJustNote "no Ent" . headMay <$> objectivesOf Objectives.entOfFangorn
      run $ PutObjectiveAllyIntoPlay ent.objectiveId pid
      toStage2 pid [Locations.steamingVent]
      inPhase QuestPhase
      q <- getActiveQuest
      run $ ActionWindow pid
      chooseAbilityOf pid q
      vent <- locationOf Locations.steamingVent
      chooseOptionMatching "exhaust the Ent" pid (const True)
      (.progress) <$> getLocation vent `shouldReturn` 2

  describe "The Voice of Saruman" do
    it "has each player draw 6 and discard down to 6, and attaches Saruman's Staff to Saruman" . isengardTest $ \pid -> do
      setUp pid
      toStage3 pid
      activeQuestCode `shouldReturn` toCardCode Quests.theVoiceOfSaruman
      handSize pid `shouldReturn` 11
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      handSize pid `shouldReturn` 6
      saruman <- enemyOf Enemies.sarumanTheRoadToIsengard
      map (.objectivePlacement) <$> objectivesOf Objectives.sarumansStaffTheRoadToIsengard `shouldReturn` [AttachedTo (EnemyTarget saruman)]
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

    it "considers Saruman engaged with the first player while Orthanc is the active location" . isengardTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      saruman <- enemyOf Enemies.sarumanTheRoadToIsengard
      getModifiers (EnemyTarget saruman) >>= (`shouldSatisfy` notElem (EngagedWithEachPlayer FirstPlayer))
      orthanc <- locationOf Locations.orthanc
      run $ PlaceLocation orthanc ActiveLocation
      getModifiers (EnemyTarget saruman) >>= (`shouldSatisfy` elem (EngagedWithEachPlayer FirstPlayer))

    it "cancels the progress placed on it this phase as Orthanc is explored, and returns Orthanc to the staging area" . isengardTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      inPhase QuestPhase
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 3
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 3
      run . Discard GameSource . LocationTarget =<< locationOf Locations.gatesOfIsengard
      orthanc <- locationOf Locations.orthanc
      run $ PlaceLocation orthanc ActiveLocation
      run $ PlaceProgressOnQuest 3
      useForcedAbilities pid
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0
      placementOfLocation orthanc `shouldReturn` StagingArea
      (.progress) <$> getLocation orthanc `shouldReturn` 0
      -- once the phase is over, progress stays again
      questProgressCanceled `shouldReturn` True
      runOnly $ EndPhase QuestPhase
      questProgressCanceled `shouldReturn` False
      questProgressThisPhase `shouldReturn` 0

    it "eliminates a player with no cards in his hand" . isengardTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      hand <- (.hand) <$> getPlayer pid
      for_ hand \c -> run $ DiscardFromHand pid c.id
      run $ CheckWindows []
      outcome `shouldReturn` Just Lost

    it "is won once defeated and Saruman has no hit points remaining" . isengardTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 15
      outcome `shouldReturn` Nothing
      saruman <- enemyOf Enemies.sarumanTheRoadToIsengard
      run $ DealDamage GameSource (EnemyTarget saruman) 5
      outcome `shouldReturn` Nothing
      run $ DealDamage GameSource (EnemyTarget saruman) 1
      outcome `shouldReturn` Just Won

  -- \* The encounter cards

  describe "Saruman's Staff" do
    it "has Saruman attack a player who draws, dealt 2 shadow cards, then heals him by 1" . isengardTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      saruman <- enemyOf Enemies.sarumanTheRoadToIsengard
      run $ DealDamage GameSource (EnemyTarget saruman) 2
      _ <- setEncounterDeck [Treacheries.machinesOfIsengard, Treacheries.machinesOfIsengard]
      run $ DrawCards (Drawing pid 1)
      length . (.shadowCards) <$> getsGame (fromJustNote "no Saruman" . lookup saruman . (.entities.enemies)) `shouldReturn` 2
      passActions pid
      questionKey pid >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))

  describe "Saruman" do
    it "has the defending player alone resolve a Wizardry shadow card dealt to him, as it turns faceup" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
      saruman <- stage Enemies.sarumanTheRoadToIsengard
      threat0 <- traverse threatOf [one, two]
      _ <- setEncounterDeck [Locations.openPit]
      run $ DealShadowCard saruman
      run $ EnemyAttack one saruman
      passAll [one, two]
      traverse threatOf [one, two] `shouldReturn` threat0
      chooseLabel "$label.takeAttackUndefended" one
      passAll [one, two]
      traverse threatOf [one, two] `shouldReturn` zipWith (+) threat0 [2, 0]

    it "resolves nothing for a Wizardry shadow card discarded before the shadow step" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
      saruman <- stage Enemies.sarumanTheRoadToIsengard
      threat0 <- traverse threatOf [one, two]
      _ <- setEncounterDeck [Locations.openPit]
      run $ DealShadowCard saruman
      run $ DiscardShadowCards saruman
      run $ EnemyAttack one saruman
      passAll [one, two]
      chooseLabel "$label.takeAttackUndefended" one
      passAll [one, two]
      traverse threatOf [one, two] `shouldReturn` threat0

    it "has the defending player assign Steaming Vent's damage, X counting him alone" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
      saruman <- stage Enemies.sarumanTheRoadToIsengard
      _ <- setEncounterDeck [Locations.steamingVent]
      run $ DealShadowCard saruman
      run $ EnemyAttack one saruman
      passAll [one, two]
      chooseLabel "$label.takeAttackUndefended" one
      passAll [one, two]
      mine <- heroOf one "01012"
      theirs <- heroOf two "01012"
      offered <- maybe [] (\q -> [t | TargetLabel t _ <- questionChoices q]) <$> getQuestion one
      offered `shouldSatisfy` elem mine
      offered `shouldSatisfy` notElem theirs
      chooseTarget one mine
      chooseTarget one mine
      getDamage mine `shouldReturn` 2
      getDamage theirs `shouldReturn` 0
      getQuestion one >>= (`shouldSatisfy` maybe True (\q -> notElem mine [t | TargetLabel t _ <- questionChoices q]))

  describe "Wolf of Isengard" do
    it "makes an additional attack after this one, as its player chooses when a Wizardry card it is dealt turns faceup" . isengardTest $ \pid -> do
      setUp pid
      wolf <- engaged pid Enemies.wolfOfIsengard
      _ <- setEncounterDeck [Locations.ringWallOfIsengard, Treacheries.blastOfSorcery]
      run $ DealShadowCard wolf
      questionKey pid >>= (`shouldSatisfy` (/= Just "$label.wolfOfIsengard.choose"))
      run $ ResolveShadow wolf pid []
      chooseLabel "$label.wolfOfIsengard.attack" pid
      getModifiers (EnemyTarget wolf) >>= (`shouldSatisfy` elem (AdditionalAttackAfterNextAttack pid))
      run $ CheckWindows [mkAfter $ EnemyAttacking wolf pid]
      passActions pid
      questionKey pid >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))
      getModifiers (EnemyTarget wolf) >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack pid))

    it "or has its player discard 1 card" . isengardTest $ \pid -> do
      setUp pid
      wolf <- engaged pid Enemies.wolfOfIsengard
      hand <- handSize pid
      _ <- setEncounterDeck [Locations.ringWallOfIsengard]
      run $ DealShadowCard wolf
      run $ ResolveShadow wolf pid []
      chooseLabel "$label.wolfOfIsengard.discard" pid
      chooseOptionMatching "discard a card" pid (const True)
      handSize pid `shouldReturn` (hand - 1)

    it "asks nothing for a Wizardry card discarded before it turns faceup" . isengardTest $ \pid -> do
      setUp pid
      wolf <- engaged pid Enemies.wolfOfIsengard
      _ <- setEncounterDeck [Locations.ringWallOfIsengard]
      run $ DealShadowCard wolf
      run $ DiscardShadowCards wolf
      run $ ResolveShadow wolf pid []
      questionKey pid >>= (`shouldSatisfy` (/= Just "$label.wolfOfIsengard.choose"))

  describe "Gríma" do
    it "is shuffled back into the encounter deck after he attacks, and the player reveals an encounter card" . isengardTest $ \pid -> do
      setUp pid
      grima <- engaged pid Enemies.grimaTheRoadToIsengard
      _ <- setEncounterDeck []
      run $ CheckWindows [mkAfter $ EnemyAttacking grima pid]
      resolveRevealed
      -- the deck held only Gríma himself, so he is what is revealed
      getEncounterDeck `shouldReturn` []
      grima' <- enemyOf Enemies.grimaTheRoadToIsengard
      placementOfEnemy grima' `shouldReturn` StagingArea

  describe "Lieutenant of Orthanc" do
    it "has the controller of a character its attack destroys discard 1 random card" . isengardTest $ \pid -> do
      setUp pid
      lieutenant <- engaged pid Enemies.lieutenantOfOrthanc
      hand <- handSize pid
      theodred <- hero "01002"
      run $ CheckWindows [mkAfter $ CharacterDestroyedByAttack lieutenant theodred pid]
      handSize pid `shouldReturn` (hand - 1)

  describe "Isengard Orc" do
    it "has its player discard 1 random card or reveal an encounter card" . isengardTest $ \pid -> do
      setUp pid
      hand <- handSize pid
      reveal [Enemies.isengardOrc, Locations.openPit]
      chooseLabel "$label.isengardOrc.discard" pid
      handSize pid `shouldReturn` (hand - 1)
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.openPit]

  describe "Machines of Isengard" do
    it "triggers the Wizardry effect of the active location" . isengardTest $ \pid -> do
      setUp pid
      pit <- stageLocation Locations.openPit
      run $ PlaceLocation pit ActiveLocation
      threat0 <- threatOf pid
      reveal [Treacheries.machinesOfIsengard, Locations.steamingVent]
      threatOf pid `shouldReturn` (threat0 + 2)
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.steamingVent]

    it "gains surge with no Wizardry effect to trigger" . isengardTest $ \pid -> do
      setUp pid
      reveal [Treacheries.machinesOfIsengard, Locations.steamingVent]
      getEncounterDeck `shouldReturn` []

  describe "Wizardry" do
    it "triggers as the location leaves play explored, active or not" . isengardTest $ \pid -> do
      setUp pid
      ringWall <- stageLocation Locations.ringWallOfIsengard
      hand <- handSize pid
      run $ PlaceTokens GameSource (LocationTarget ringWall) Progress 2
      useForcedAbilities pid
      handSize pid `shouldReturn` (hand - 1)

  describe "Open Pit" do
    it "raises the threat of a player who draws from a player card effect, and only then" . isengardTest $ \pid -> do
      setUp pid
      toStage2 pid [Locations.openPit]
      threat0 <- threatOf pid
      run $ DrawCards (Drawing pid 1)
      threatOf pid `shouldReturn` threat0
      card <- putCardIntoHand pid Allies.guardOfTheCitadel
      run $ DrawCards (DrawingFrom pid 1 (CardIdSource card.id))
      useForcedAbilities pid
      threatOf pid `shouldReturn` (threat0 + 2)

  describe "Steaming Vent" do
    it "has a player who commits characters deal 1 damage to one of them" . isengardTest $ \pid -> do
      setUp pid
      _ <- stageLocation Locations.steamingVent
      beravor <- hero "01012"
      run $ CommitToQuest beravor
      run $ CheckWindows [mkAfter $ CommittedToQuest beravor]
      getDamage beravor `shouldReturn` 1

  describe "Underground Armoury" do
    it "deals a shadow card to an enemy as it engages a player" . isengardTest $ \pid -> do
      setUp pid
      _ <- stageLocation Locations.undergroundArmoury
      _ <- setEncounterDeck [Treacheries.blastOfSorcery]
      orc <- engaged pid Enemies.isengardOrc
      length . (.shadowCards) <$> getsGame (fromJustNote "no orc" . lookup orc . (.entities.enemies)) `shouldReturn` 1

  describe "Plain of Isengard" do
    it "has 1 threat for each player and sends archery damage to allies" . isengardTest $ \pid -> do
      setUp pid
      plain <- stageLocation Locations.plainOfIsengard
      getLocationThreat plain `shouldReturn` 1
      getModifiers GameTarget >>= (`shouldSatisfy` elem ArcheryDamageToAllies)

  describe "Gates of Isengard" do
    it "has each player put an enemy into play engaged with him as it is explored" . isengardTest $ \pid -> do
      setUp pid
      toStage2 pid [Locations.openPit]
      _ <- setEncounterDeck [Enemies.lieutenantOfOrthanc]
      gates <- locationOf Locations.gatesOfIsengard
      run $ PlaceTokens GameSource (LocationTarget gates) Progress 1
      useForcedAbilities pid
      chooseOptionMatching "take the Lieutenant" pid (const True)
      lieutenant <- enemyOf Enemies.lieutenantOfOrthanc
      placementOfEnemy lieutenant `shouldReturn` EngagedWith pid

  describe "Orthanc" do
    it "has the first player reveal 1 of the top 3 cards after the players quest successfully, discarding the others" . isengardTest $ \pid -> do
      setUp pid
      toStage2 pid [Locations.openPit]
      vent : _ <- setEncounterDeck [Locations.steamingVent, Locations.plainOfIsengard, Locations.ringWallOfIsengard]
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      useForcedAbilities pid
      chooseTarget pid (CardIdTarget vent.id)
      resolveRevealed
      (placementOfLocation =<< locationOf Locations.steamingVent) `shouldReturn` StagingArea
      getEncounterDeck `shouldReturn` []
      length <$> getEncounterDiscard `shouldReturn` 2

  describe "Blast of Sorcery" do
    it "assigns 1 more damage than there are players among the characters committed to the quest" . isengardTest $ \pid -> do
      setUp pid
      beravor <- hero "01012"
      run $ CommitToQuest beravor
      reveal [Treacheries.blastOfSorcery]
      chooseTarget pid beravor
      chooseTarget pid beravor
      getDamage beravor `shouldReturn` 2

  describe "Poisoned Vapour" do
    it "exhausts and blanks each damaged character, and surges" . isengardTest $ \pid -> do
      setUp pid
      beravor <- hero "01012"
      run $ DealDamage GameSource beravor 1
      reveal [Treacheries.poisonedVapour, Locations.openPit]
      getModifiers beravor >>= (`shouldSatisfy` elem Blank)
      heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
      getEncounterDeck `shouldReturn` []

    it "blanks whichever characters are damaged until the end of the combat phase" . isengardTest $ \pid -> do
      setUp pid
      beravor <- hero "01012"
      eowyn <- hero "01007"
      run $ DealDamage GameSource beravor 1
      reveal [Treacheries.poisonedVapour, Locations.openPit]
      run $ HealDamage GameSource beravor 1
      getModifiers beravor >>= (`shouldSatisfy` notElem Blank)
      heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
      getModifiers eowyn >>= (`shouldSatisfy` notElem Blank)
      run $ DealDamage GameSource eowyn 1
      getModifiers eowyn >>= (`shouldSatisfy` elem Blank)
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False
      inPhase CombatPhase
      runOnly $ EndPhase CombatPhase
      getModifiers eowyn >>= (`shouldSatisfy` notElem Blank)

  describe "Saruman's Voice" do
    it "removes a questing hero from the quest and has its controller discard as it defends" . campaignTest $ \pid -> do
      setUp pid
      beravor <- hero "01012"
      run $ CommitToQuest beravor
      threat0 <- threatOf pid
      reveal [Treacheries.sarumansVoice]
      threatOf pid `shouldReturn` (threat0 + 2)
      voice <- getsGame (fromJustNote "no Voice" . find ((== toCardCode Treacheries.sarumansVoice) . toCardCode) . toList . (.entities.treacheries))
      voice.placement `shouldBe` AttachedTo beravor
      heroCommitted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
      hand <- handSize pid
      lurker <- engaged pid Enemies.isengardOrc
      run $ CheckWindows [mkAfter $ DeclaredAsDefender beravor lurker]
      chooseOptionMatching "discard a card" pid (const True)
      handSize pid `shouldReturn` (hand - 1)

    it "is discarded, attaching to no one, with no hero committed to the quest" . campaignTest $ \pid -> do
      setUp pid
      reveal [Treacheries.sarumansVoice]
      questionKey pid >>= (`shouldSatisfy` (/= Just "$label.sarumansVoice.attach"))
      getsGame (any ((== toCardCode Treacheries.sarumansVoice) . toCardCode) . toList . (.entities.treacheries)) `shouldReturn` False
      copies Treacheries.sarumansVoice . (.encounterDiscard) <$> scenarioAttrs `shouldReturn` 1

  -- \* The campaign

  describe "the campaign resolution" do
    it "earns the Palantír of Orthanc with more than 3 cards in hand" . campaignTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      winGameNow
      l <- getCampaignLog
      [p | p <- l.pool, p.code `elem` [toCardCode Treacheries.sarumansVoice, toCardCode Attachments.palantirOfOrthanc]]
        `shouldBe` [PoolCard (toCardCode Attachments.palantirOfOrthanc) (Just 0) Nothing]

    it "has the first player choose the player who earns the Palantír of Orthanc" . twoPlayerCampaignTestIn theRoadToIsengard lotr {campaignLogHeroes = [["01012", "01007", "01002"], ["01012", "01007", "01002"]]} testDeck $ \one two -> do
      run $ DrawCards $ DrawingFrom one 4 GameSource
      run $ GameOver Won
      getsGame (.outcome) `shouldReturn` Nothing
      chooseTarget one (PlayerTarget two)
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      [p | p <- l.pool, p.code == toCardCode Attachments.palantirOfOrthanc]
        `shouldBe` [PoolCard (toCardCode Attachments.palantirOfOrthanc) (Just 1) Nothing]

    it "earns both with exactly 3 cards in hand" . campaignTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      discardDownTo pid 3
      winGameNow
      l <- getCampaignLog
      [p.code | p <- l.pool, p.code `elem` [toCardCode Treacheries.sarumansVoice, toCardCode Attachments.palantirOfOrthanc]]
        `shouldBe` [toCardCode Treacheries.sarumansVoice, toCardCode Attachments.palantirOfOrthanc]

    it "earns Saruman's Voice alone with fewer than 3 cards in hand" . campaignTest $ \pid -> do
      setUp pid
      toStage3 pid
      replicateM_ 5 $ chooseOptionMatching "discard" pid (const True)
      discardDownTo pid 2
      winGameNow
      l <- getCampaignLog
      [p | p <- l.pool, p.code `elem` [toCardCode Treacheries.sarumansVoice, toCardCode Attachments.palantirOfOrthanc]]
        `shouldBe` [PoolCard (toCardCode Treacheries.sarumansVoice) Nothing Nothing]
      map completedScenario l.completed `shouldSatisfy` elem "the-road-to-isengard"
 where
  -- stage 3 defeated and Saruman out of hit points
  winGameNow = do
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 15
    saruman <- enemyOf Enemies.sarumanTheRoadToIsengard
    run $ DealDamage GameSource (EnemyTarget saruman) 6
    outcome `shouldReturn` Just Won
  discardDownTo pid n = do
    hand <- (.hand) <$> getPlayer pid
    for_ (drop n hand) \c -> run $ DiscardFromHand pid c.id

