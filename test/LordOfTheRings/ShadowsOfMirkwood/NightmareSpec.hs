module LordOfTheRings.ShadowsOfMirkwood.NightmareSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Modifier (ModifierType (CannotHaveAttachments, CannotHavePlayerAttachments, CannotReduceThreat, LoseKeyword))
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Phase
import LordOfTheRings.Scenario (aJourneyToRhosgobel, conflictAtTheCarrock, returnToMirkwood, theDeadMarshes, theHillsOfEmynMuil, theHuntForGollum)
import LordOfTheRings.Scenario.Types (GameMode (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (Explored, PhaseEnds), mkAfter, mkWhen)
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

theLocation :: GameT LocationId
theLocation = getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

theEnemy :: GameT EnemyId
theEnemy = getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))

enemyCardId :: EnemyId -> GameT CardId
enemyCardId eid = getsGame (fromJustNote "no enemy" . fmap (.cardId) . lookup eid . map (\e -> (toId e, toAttrs e)) . toList . (.entities.enemies))

objectives :: GameT [ObjectiveAttrs]
objectives = getsGame (map toAttrs . toList . (.entities.objectives))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

spec :: Spec
spec = describe "Shadows of Mirkwood nightmare mode" do
  describe "The Hunt for Gollum" do
    it "builds the nightmare deck" . nightmare theHuntForGollum $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 47
      map (`copies` deck) [Enemies.mistyMountainGoblins, Locations.gladdenFields, Enemies.ravenousHawk, Treacheries.restlessHunters]
        `shouldBe` [1, 0, 3, 3]

    it "attaches a free Clue card to a Mordor enemy at the end of the refresh phase" . nightmare theHuntForGollum $ \_ -> do
      reveal Enemies.huntingWarg []
      warg <- enemyCardId =<< theEnemy
      run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.signsOfGollum
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      map objectiveGuard <$> objectives `shouldReturn` [Just warg]

    it "loses the game with four Clue cards on Mordor enemies" . nightmare theHuntForGollum $ \_ -> do
      reveal Enemies.huntingWarg []
      warg <- enemyCardId =<< theEnemy
      replicateM_ 4 $ run . (`PutObjectiveIntoPlayGuardedBy` warg) =<< genEncounterCard Objectives.signsOfGollum
      getsGame (.outcome) `shouldReturn` Just Lost

    it "The Enemy's Trail attaches a Clue card from the discard pile to a Mordor enemy" . nightmare theHuntForGollum $ \_ -> do
      reveal Enemies.huntingWarg []
      warg <- enemyCardId =<< theEnemy
      run . AddToEncounterDiscard . pure =<< genEncounterCard Objectives.signsOfGollum
      reveal Treacheries.theEnemysTrail []
      map objectiveGuard <$> objectives `shouldReturn` [Just warg]

    it "The Trail Goes Cold makes the active location count in the quest phase" . nightmare theHuntForGollum $ \_ -> do
      reveal Locations.darkPools []
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ TravelTo lid
      reveal Treacheries.theTrailGoesCold []
      run $ BeginPhase QuestPhase
      getStagingThreat `shouldReturn` 3

    it "Ravenous Hawk engages the player who looks at it" . nightmare theHuntForGollum $ \pid -> do
      _ <- setEncounterDeck [Enemies.ravenousHawk]
      run $ LookAtTopOfEncounterDeck pid GameSource False
      eid <- theEnemy
      getsGame (fmap (.placement) . lookup eid . map (\e -> (toId e, toAttrs e)) . toList . (.entities.enemies))
        `shouldReturn` Just (EngagedWith pid)

    it "Ravenous Hawk engages the player who looks at it among several cards" . nightmare theHuntForGollum $ \pid -> do
      _ <- setEncounterDeck [Enemies.mistyMountainGoblins, Enemies.ravenousHawk]
      run $ RevealOneOfTopEncounterCards pid GameSource 2 False
      eid <- getsGame (fromJustNote "no hawk" . fmap toId . find ((== toCardCode Enemies.ravenousHawk) . toCardCode) . toList . (.entities.enemies))
      getsGame (fmap (.placement) . lookup eid . map (\e -> (toId e, toAttrs e)) . toList . (.entities.enemies))
        `shouldReturn` Just (EngagedWith pid)

  describe "Conflict at the Carrock" do
    it "builds the nightmare deck, its own Trolls in place of the standard ones" . nightmare conflictAtTheCarrock $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 43
      map (`copies` deck) [Enemies.louis, Enemies.louisNightmare, Enemies.robAndBob, Enemies.savageHillTroll, Locations.beePastures]
        `shouldBe` [0, 1, 1, 4, 0]

    it "places 1 progress on the quest when the players placed none this quest phase" . nightmare conflictAtTheCarrock $ \_ -> do
      _ <- putQuestStage Quests.grimbeornsQuest
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      (.progress) <$> getActiveQuest `shouldReturn` 1

    it "has each Troll refuse player attachments only while Stuart is in the victory display" . nightmare conflictAtTheCarrock $ \_ -> do
      reveal Enemies.stuartNightmare []
      stuart <- theEnemy
      run $ AddToVictoryDisplay (toTarget stuart)
      reveal Enemies.savageHillTroll []
      troll <- theEnemy
      mods <- getModifiers troll
      mods `shouldSatisfy` elem CannotHavePlayerAttachments
      mods `shouldSatisfy` notElem CannotHaveAttachments

    it "gives each Troll +1 attack while Morris is in the victory display" . nightmare conflictAtTheCarrock $ \_ -> do
      reveal Enemies.morrisNightmare []
      morris <- theEnemy
      run $ AddToVictoryDisplay (toTarget morris)
      reveal Enemies.savageHillTroll []
      troll <- theEnemy
      getEnemyAttack troll `shouldReturn` 4

    it "Grimbeorn's Folly adds Rob & Bob and a Troll, and is won when Rob & Bob fall" . nightmare conflictAtTheCarrock $ \pid -> do
      run . SetAside =<< genEncounterCard Enemies.robAndBob
      _ <- setEncounterDeck [Enemies.savageHillTroll]
      folly <- putQuestStage Quests.grimbeornsFolly
      run $ WhenRevealed folly
      chooseOptionMatching "the Troll found" pid (const True)
      names <- getsGame (map toCardCode . toList . (.entities.enemies))
      sort names `shouldBe` sort (map toCardCode [Enemies.robAndBob, Enemies.savageHillTroll])
      robAndBob <- getsGame (fromJustNote "no Rob & Bob" . headMay . map toId . filter ((== toCardCode Enemies.robAndBob) . toCardCode) . toList . (.entities.enemies))
      run $ Destroy GameSource (toTarget robAndBob)
      getsGame (.outcome) `shouldReturn` Just Won

    it "Time for Supper heals the Trolls" . nightmare conflictAtTheCarrock $ \_ -> do
      reveal Enemies.savageHillTroll []
      troll <- theEnemy
      run $ DealDamage GameSource (toTarget troll) 2
      reveal Treacheries.timeForSupper []
      getDamage (toTarget troll) `shouldReturn` 0

    it "Stuart bars only player attachments" . nightmare conflictAtTheCarrock $ \_ -> do
      run . AddToStaging =<< genEncounterCard Enemies.stuartNightmare
      stuart <- theEnemy
      getModifiers stuart >>= (`shouldSatisfy` \ms -> CannotHavePlayerAttachments `elem` ms && CannotHaveAttachments `notElem` ms)

  describe "A Journey to Rhosgobel" do
    it "builds the nightmare deck" . nightmare aJourneyToRhosgobel $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 54
      map (`copies` deck) [Locations.forestGrove, Enemies.goblinEagleHunter, Treacheries.noxiousSupplies]
        `shouldBe` [0, 3, 3]

    it "adds a Goblin Eagle-hunter to the staging area during setup" . nightmare aJourneyToRhosgobel $ \_ -> do
      run $ DoStep 1 Setup
      run $ DoStep 8 Setup
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.goblinEagleHunter]

    it "gives the first Creature revealed in the quest phase surge" . nightmare aJourneyToRhosgobel $ \_ -> do
      putGame . (phaseL .~ QuestPhase) =<< getGame
      reveal Enemies.treetopBirdEater [Locations.labyrinthineWoods, Locations.darkWoodGrove]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "Noxious Supplies heals Wilyador of 1 damage only, then is discarded" . nightmare aJourneyToRhosgobel $ \_ -> do
      run . AddToStaging =<< genEncounterCard Objectives.wilyador
      w <- getsGame (toTarget . fromJustNote "no Wilyador" . find ((== toCardCode Objectives.wilyador) . toCardCode) . toList . (.entities.allies))
      run $ DealDamage GameSource w 5
      reveal Treacheries.noxiousSupplies [Locations.darkWoodGrove]
      run $ HealDamage GameSource w 3
      getDamage w `shouldReturn` 4
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0

    it "Noxious Supplies does not limit healing by the player card Athelas" . nightmare aJourneyToRhosgobel $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.wilyador
      w <- getsGame (toTarget . fromJustNote "no Wilyador" . find ((== toCardCode Objectives.wilyador) . toCardCode) . toList . (.entities.allies))
      run $ DealDamage GameSource w 5
      reveal Treacheries.noxiousSupplies [Locations.darkWoodGrove]
      hero <- getsGame (toTarget . fromJustNote "no hero" . headMay . toList . (.entities.heroes))
      AttachmentTarget athelas <- putAttachmentOn pid Attachments.athelas hero
      run $ HealDamage (AbilitySource (AttachmentSource athelas) 1) w 3
      getDamage w `shouldReturn` 2

    it "Labyrinthine Woods gives Creatures +1 attack in the staging area" . nightmare aJourneyToRhosgobel $ \_ -> do
      reveal Locations.labyrinthineWoods []
      reveal Enemies.treetopBirdEater []
      (getEnemyAttack =<< theEnemy) `shouldReturn` 5

  describe "The Hills of Emyn Muil" do
    it "builds the nightmare deck" . nightmare theHillsOfEmynMuil $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 46
      map (`copies` deck) [Locations.theHighlands, Enemies.dolGuldurOrcs, Enemies.tunnelingNamelessThing]
        `shouldBe` [0, 1, 3]

    it "places no more than 1 progress on a location in the staging area each round" . nightmare theHillsOfEmynMuil $ \_ -> do
      reveal Locations.sarnGebirRapids []
      lid <- theLocation
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      run $ PlaceTokens GameSource (toTarget lid) Progress 1
      (.progress) <$> getLocation lid `shouldReturn` 1

    it "Orc Interceptor grows as locations are explored" . nightmare theHillsOfEmynMuil $ \_ -> do
      reveal Enemies.orcInterceptor []
      reveal Locations.sarnGebirRapids []
      lid <- theLocation
      run $ CheckWindows [mkAfter $ Explored lid]
      (getEnemyAttack =<< theEnemy) `shouldReturn` 3

    it "Retraced Steps discards an Emyn Muil location from the victory display" . nightmare theHillsOfEmynMuil $ \_ -> do
      reveal Locations.bramblesAndThorns []
      lid <- theLocation
      run $ AddToVictoryDisplay (toTarget lid)
      reveal Treacheries.retracedSteps [Enemies.orcInterceptor]
      getsGame ((.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` []
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.bramblesAndThorns))

    it "A Hurrying Darkness raises a player's threat by 5 if he chooses" . nightmare theHillsOfEmynMuil $ \pid -> do
      was <- threat pid
      reveal Treacheries.aHurryingDarkness []
      chooseLabel "$label.aHurryingDarkness.threat" pid
      threat pid `shouldReturn` was + 5

  describe "The Dead Marshes" do
    it "builds the nightmare deck" . nightmare theDeadMarshes $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 45
      map (`copies` deck) [Locations.fensAndMires, Enemies.giantMarshWorm, Locations.theMereOfDeadFaces, Enemies.thingsInThePools]
        `shouldBe` [0, 2, 1, 3]

    it "keeps the players from reducing their threat" . nightmare theDeadMarshes $ \pid -> do
      run $ DoStep 1 Setup
      getModifiers pid >>= (`shouldSatisfy` elem CannotReduceThreat)

    it "The Mere of Dead Faces holds Gollum's resources until he enters play" . nightmare theDeadMarshes $ \_ -> do
      reveal Locations.theMereOfDeadFaces []
      mere <- theLocation
      run $ PlaceTokens GameSource (toTarget mere) Resource 3
      getStagingThreat `shouldReturn` 3
      run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.gollumTheDeadMarshes
      map (.resources) <$> objectives `shouldReturn` [5]
      getStagingThreat `shouldReturn` 0

    it "shuffles Gollum back into the encounter deck from the discard pile" . nightmare theDeadMarshes $ \_ -> do
      _ <- setEncounterDeck [Enemies.thingsInThePools]
      run . AddToEncounterDiscard . pure =<< genEncounterCard Objectives.gollumTheDeadMarshes
      getEncounterDiscard `shouldReturn` []
      length <$> getEncounterDeck `shouldReturn` 2

    it "Things in the Pools damages each character committed to an escape test" . nightmare theDeadMarshes $ \pid -> do
      reveal Enemies.thingsInThePools []
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ MakeEscapeTest $ EscapeTest pid GameSource 1 EscapeWillpower [] [] [] []
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 1

    it "Ill-fated Guard lets its player commit only 1 character to an escape test" . nightmare theDeadMarshes $ \pid -> do
      reveal Enemies.illFatedGuard []
      run . EngageEnemy pid =<< theEnemy
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ MakeEscapeTest $ EscapeTest pid GameSource 1 EscapeWillpower [] [] [] []
      chooseTarget pid eowyn
      Just question <- getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldBe` []

    it "Devilry of the Dark Land returns Undead enemies from the discard pile" . nightmare theDeadMarshes $ \_ -> do
      run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.marshWight
      reveal Treacheries.devilryOfTheDarkLand []
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.marshWight]

    it "Lost Soul of Lórien destroyed attaches to a hero as a condition, its willpower 0" . nightmare theDeadMarshes $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.lostSoulOfLorien
      soul <- theEnemy
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ DealDamage GameSource (EnemyTarget soul) 3
      chooseTarget pid eowyn
      getsGame (fmap (.placement) . find ((== soul) . toId) . toList . (.entities.enemies)) `shouldReturn` Just (AttachedTo eowyn)
      getWillpower eowyn `shouldReturn` 0

  describe "Return to Mirkwood" do
    it "builds the nightmare deck" . nightmare returnToMirkwood $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 53
      map (`copies` deck) [Locations.woodmansGlade, Enemies.goblinSniper, Enemies.webSpinner]
        `shouldBe` [0, 0, 3]

    it "adds Attercop, Attercop to the staging area as stage 4A is revealed" . nightmare returnToMirkwood $ \pid -> do
      _ <- setEncounterDeck [Enemies.attercopAttercop]
      run . AdvanceToQuest =<< genEncounterCard Quests.ambush
      chooseOptionMatching "Attercop, Attercop" pid (const True)
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.attercopAttercop]

    it "Isolation takes the ranged keyword from every character" . nightmare returnToMirkwood $ \_ -> do
      _ <- putQuestStage Quests.throughTheForest
      reveal Treacheries.isolation [Locations.webWall]
      (getModifiers =<< getHeroWithCode "01007") >>= (`shouldSatisfy` elem (LoseKeyword Ranged))

    it "Web-wall reveals the topmost Spider enemy in the discard pile" . nightmare returnToMirkwood $ \_ -> do
      run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.attercopAttercop
      reveal Locations.webWall []
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.attercopAttercop]

    it "Deceptive Path gives way to the next location in the encounter deck" . nightmare returnToMirkwood $ \_ -> do
      reveal Locations.deceptivePath []
      path <- theLocation
      _ <- setEncounterDeck [Treacheries.isolation, Locations.webWall]
      run $ TravelTo path
      getsGame (map (\l -> (toCardCode l, (toAttrs l).placement)) . toList . (.entities.locations))
        `shouldReturn` [(toCardCode Locations.webWall, ActiveLocation)]
