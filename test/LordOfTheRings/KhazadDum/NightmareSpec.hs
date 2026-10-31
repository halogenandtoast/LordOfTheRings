module LordOfTheRings.KhazadDum.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Scenario (intoThePit, theSeventhLevel)
import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (resourcesOn)
import LordOfTheRings.Scenario.Types (GameMode (..), modeL, scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

codes :: [CardDef] -> [CardCode]
codes = map toCardCode

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemies :: GameT [CardCode]
enemies = getsGame (sort . map toCardCode . toList . (.entities.enemies))

questDeck :: GameT [CardCode]
questDeck = getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario))

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

active :: CardDef -> GameT LocationId
active def = do
  run . PutIntoPlayAsActiveLocation =<< genEncounterCard def
  locationOf def

-- | Cave Torch on Beravor, as its setup attaches it.
caveTorch :: GameT Target
caveTorch = do
  beravor <- toTarget <$> getHeroWithCode "01012"
  run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.caveTorch
  getsGame (fromJustNote "no torch" . fmap toTarget . headMay . toList . (.entities.objectives))

-- | Trigger the forced abilities waiting on the player, in the order offered.
forcedAbilities :: PlayerId -> GameT ()
forcedAbilities pid = do
  q <- getQuestion pid
  when (any (any (\case AbilityLabel {} -> True; _ -> False) . questionChoices) q) do
    chooseAbility pid
    forcedAbilities pid

spec :: Spec
spec = describe "Khazad-dûm Nightmare mode" do
  describe "Into the Pit" do
    it "builds the nightmare deck, Goblin Ambush in Goblin Patrol's place" . nightmare intoThePit $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 54
      map (`copies` deck) [Enemies.goblinSwordsman, Enemies.goblinFollower, Enemies.patrolSentry, Locations.fouledWell]
        `shouldBe` [0, 2, 4, 1]
      questDeck `shouldReturn` codes [Quests.goblinAmbush, Quests.aWayUp]

    it "removes Cave Torch from the game once it has exhausted 5 times" . nightmare intoThePit $ \pid -> do
      torch <- caveTorch
      for_ [1 .. 5 :: Int] \_ -> do
        _ <- setEncounterDeck [Treacheries.shadowFall]
        run $ Exhaust torch
        -- the setup card's Forced and Cave Torch's own, in the order the player picks
        forcedAbilities pid
        run $ Ready torch
      getsGame (length . (.entities.objectives)) `shouldReturn` 0

    it "Patrol Sentry attacks its player as Cave Torch exhausts" . nightmare intoThePit $ \pid -> do
      torch <- caveTorch
      reveal Enemies.patrolSentry []
      eid <- enemyOf Enemies.patrolSentry
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Treacheries.shadowFall, Treacheries.shadowFall]
      run $ Exhaust torch
      forcedAbilities pid
      -- dealt its shadow card, it asks for a defender
      getsGame (maybe 0 (length . (.shadowCards)) . lookup eid . (.entities.enemies)) `shouldReturn` 1

    it "Goblin Lurker gets +3 threat while the active location is Dark" . nightmare intoThePit $ \_ -> do
      reveal Enemies.goblinLurker []
      eid <- enemyOf Enemies.goblinLurker
      getEnemyThreat eid `shouldReturn` 3
      _ <- active Locations.lightlessPassage
      getEnemyThreat eid `shouldReturn` 6

    it "Shadow Dweller's shadow shuffles it back into the encounter deck" . nightmare intoThePit $ \pid -> do
      reveal Enemies.goblinLurker []
      eid <- enemyOf Enemies.goblinLurker
      _ <- setEncounterDeck []
      shadow pid eid Enemies.shadowDweller []
      map toCardCode <$> getEncounterDeck `shouldReturn` codes [Enemies.shadowDweller]

    it "Twisting Shadows counts the Dark locations, and clears their progress" . nightmare intoThePit $ \_ -> do
      corridor <- active Locations.lightlessPassage
      run $ PlaceTokens GameSource (toTarget corridor) Progress 2
      reveal Locations.twistingShadows []
      shadows <- locationOf Locations.twistingShadows
      getLocationQuestPoints shadows `shouldReturn` 2
      getsGame (fmap (.progress) . lookup corridor . (.entities.locations)) `shouldReturn` Just 0

    it "Narrow Fissure adds 2 quest points to Dark locations, and takes no progress while Cave Torch is ready" . nightmare intoThePit $ \pid -> do
      _ <- caveTorch
      corridor <- active Locations.lightlessPassage
      was <- getLocationQuestPoints corridor
      reveal Locations.narrowFissure []
      getLocationQuestPoints corridor `shouldReturn` was + 2
      fissure <- locationOf Locations.narrowFissure
      run $ PlaceTokens (PlayerSource pid) (toTarget fissure) Progress 1
      getsGame (fmap (.progress) . lookup fissure . (.entities.locations)) `shouldReturn` Just 0

    it "Shadow-fall makes a location Dark, with 3 more quest points" . nightmare intoThePit $ \_ -> do
      _ <- active Locations.firstHall
      hall <- locationOf Locations.firstHall
      was <- getLocationQuestPoints hall
      -- the only location that is not Dark takes it
      reveal Treacheries.shadowFall [Enemies.goblinLurker]
      getLocationQuestPoints hall `shouldReturn` was + 3
      eid <- enemyOf Enemies.goblinLurker
      getEnemyThreat eid `shouldReturn` 6

    it "Goblin Ambush brings out Patrol Sentry and Patrol Leader, and takes progress from defeated enemies only" . nightmare intoThePit $ \pid -> do
      [leader, sentry, _] <- setEncounterDeck [Enemies.patrolLeader, Enemies.patrolSentry, Enemies.goblinLurker]
      stage <- putQuestStage Quests.goblinAmbush
      run $ Flip stage
      chooseTarget pid stage
      -- X is 2 with 1 player: Patrol Sentry, then Patrol Leader
      chooseTarget pid sentry
      chooseTarget pid leader
      enemies `shouldReturn` sort (codes [Enemies.patrolLeader, Enemies.patrolSentry])
      run $ PlaceTokens GameSource stage Progress 1
      (.progress) <$> getActiveQuest `shouldReturn` 0
      eid <- enemyOf Enemies.patrolSentry
      run $ DealDamage GameSource (toTarget eid) 5
      (.progress) <$> getActiveQuest `shouldReturn` 1

    it "Goblin Lurker's shadow gives +3 attack under a Dark active location" . nightmare intoThePit $ \pid -> do
      _ <- active Locations.lightlessPassage
      reveal Enemies.patrolSentry []
      eid <- enemyOf Enemies.patrolSentry
      shadow pid eid Enemies.goblinLurker []
      getEnemyAttack eid `shouldReturn` 7

  describe "The Seventh Level" do
    it "builds the nightmare deck, Overseer Maurûl and Chamber of Records set aside" . nightmare theSeventhLevel $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 40
      map (`copies` deck) [Enemies.goblinSwordsman, Enemies.goblinScout, Enemies.overseerMaurul, Treacheries.balinsDoom]
        `shouldBe` [3, 1, 0, 3]
      aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
      sort aside `shouldBe` sort (codes [Enemies.overseerMaurul, Locations.chamberOfRecords])

    it "makes Chamber of Records active and brings out Overseer Maurûl as stage 2B is revealed" . nightmare theSeventhLevel $ \_ -> do
      traverse_ (run . SetAside <=< genEncounterCard) [Enemies.overseerMaurul, Locations.chamberOfRecords]
      upper <- active Locations.upperHall
      doom <- genEncounterCard Treacheries.balinsDoom
      run $ AddToEncounterDiscard [doom]
      _ <- setEncounterDeck []
      stage <- putQuestStage Quests.theFateOfBalin
      run $ Flip stage
      chamber <- locationOf Locations.chamberOfRecords
      getsGame (fmap (.placement) . lookup chamber . (.entities.locations)) `shouldReturn` Just ActiveLocation
      getsGame (fmap (.placement) . lookup upper . (.entities.locations)) `shouldReturn` Just StagingArea
      enemies `shouldReturn` codes [Enemies.overseerMaurul]
      map toCardCode <$> getEncounterDeck `shouldReturn` codes [Treacheries.balinsDoom]

    it "wins at The Fate of Balin only with Overseer Maurûl in the victory display" . nightmare theSeventhLevel $ \pid -> do
      reveal Enemies.overseerMaurul []
      stage <- putQuestStage Quests.theFateOfBalin
      run $ Flip stage
      chooseTarget pid stage
      run $ PlaceTokens GameSource stage Progress 17
      getsGame (.outcome) `shouldReturn` Nothing
      overseer <- enemyOf Enemies.overseerMaurul
      run $ DealDamage GameSource (toTarget overseer) 12
      getsGame (.outcome) `shouldReturn` Just Won

    it "Overseer Maurûl gets the most resources on a Goblin to each stat, and Goblin Skirmisher once to attack" . nightmare theSeventhLevel $ \_ -> do
      reveal Enemies.overseerMaurul []
      reveal Enemies.pitGoblin []
      overseer <- enemyOf Enemies.overseerMaurul
      pit <- enemyOf Enemies.pitGoblin
      resourcesOn pit `shouldReturn` 2
      getEnemyThreat overseer `shouldReturn` 5
      getEnemyDefense overseer `shouldReturn` 5
      reveal Enemies.goblinSkirmisher []
      reveal Enemies.goblinSkirmisher []
      getEnemyAttack pit `shouldReturn` 4

    it "Western Hall gives Goblins defense for their resources, and resources as the players travel there" . nightmare theSeventhLevel $ \_ -> do
      reveal Enemies.goblinScout []
      reveal Locations.westernHall []
      scout <- enemyOf Enemies.goblinScout
      hall <- locationOf Locations.westernHall
      run $ TravelTo hall
      resourcesOn scout `shouldReturn` 1
      getEnemyDefense scout `shouldReturn` 1

    it "Orc Taskmaster takes a resource from a Goblin to cancel its damage" . nightmare theSeventhLevel $ \_ -> do
      reveal Enemies.goblinScout []
      reveal Enemies.orcTaskmaster []
      scout <- enemyOf Enemies.goblinScout
      resourcesOn scout `shouldReturn` 1
      run $ DealDamage GameSource (toTarget scout) 1
      resourcesOn scout `shouldReturn` 0
      getsGame (fmap (.damage) . lookup scout . (.entities.enemies)) `shouldReturn` Just 0

    it "Chamber of Records gives a Goblin 2 resources as it engages" . nightmare theSeventhLevel $ \pid -> do
      _ <- active Locations.chamberOfRecords
      reveal Enemies.goblinScout []
      scout <- enemyOf Enemies.goblinScout
      run $ EngageEnemy pid scout
      resourcesOn scout `shouldReturn` 2

    it "Deep-mines in the staging area gives each Goblin entering it a resource" . nightmare theSeventhLevel $ \_ -> do
      reveal Locations.deepMines []
      reveal Enemies.goblinScout []
      (resourcesOn =<< enemyOf Enemies.goblinScout) `shouldReturn` 1

    it "Balin's Doom adds a resource to each Goblin" . nightmare theSeventhLevel $ \_ -> do
      reveal Enemies.goblinScout []
      reveal Treacheries.balinsDoom [Locations.upperHall]
      (resourcesOn =<< enemyOf Enemies.goblinScout) `shouldReturn` 1

    it "Beats and Echoes reveals 2 more cards than players engaged with Goblins, enemies to the staging area" . nightmare theSeventhLevel $ \_ -> do
      reveal Treacheries.beatsAndEchoes [Enemies.goblinScout, Locations.upperHall, Enemies.pitGoblin, Enemies.orcTaskmaster]
      enemies `shouldReturn` sort (codes [Enemies.goblinScout, Enemies.pitGoblin])
      length <$> getEncounterDeck `shouldReturn` 1
