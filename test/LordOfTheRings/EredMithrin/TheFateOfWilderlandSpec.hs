module LordOfTheRings.EredMithrin.TheFateOfWilderlandSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getEngagementCost)
import LordOfTheRings.Helpers.Character (getDefense)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (Objective)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theFateOfWilderland)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

fateTest :: (PlayerId -> GameT ()) -> IO ()
fateTest = gameTestIn theFateOfWilderland testDeck

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

objectiveOf :: CardDef -> GameT Objective
objectiveOf def = getsGame (fromJustNote "no such objective" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

-- | The Goblins' Assault, The Heroes' Defense and Urdug in the staging area, as stage 1A
-- puts them.
board :: GameT EnemyId
board = do
  for_ [Objectives.theGoblinsAssault, Objectives.theHeroesDefense] \o -> run . AddToStaging =<< genEncounterCard o
  stage Enemies.urdug

assault :: GameT Target
assault = toTarget <$> objectiveOf Objectives.theGoblinsAssault

assaultProgress :: GameT Int
assaultProgress = (.progress) <$> objectiveOf Objectives.theGoblinsAssault

defense :: GameT Target
defense = toTarget <$> objectiveOf Objectives.theHeroesDefense

defenseResources :: GameT Int
defenseResources = (.resources) <$> objectiveOf Objectives.theHeroesDefense

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

-- | The stage put into play turned to its side B, its When Revealed effect resolved.
flippedStage :: PlayerId -> CardDef -> GameT ()
flippedStage pid def = do
  q <- putQuestStage def
  run $ Flip q
  getQuestion pid >>= \case
    Just (QuestionLabel l _ _ _) | "$label.resolveWhenRevealed" `isPrefixOf` l -> resolveWhenRevealed pid
    _ -> pure ()

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

encounterDiscardCodes :: GameT [CardCode]
encounterDiscardCodes = map toCardCode <$> getEncounterDiscard

spec :: Spec
spec = describe "The Fate of Wilderland" do
  it "is a scenario the game can be dealt" . fateTest $ \_ ->
    isJust (lookupScenario "the-fate-of-wilderland") `shouldBe` True

  describe "setup" do
    it "adds The Goblins' Assault, The Heroes' Defense, Urdug and an enemy per player to the staging area" . fateTest $ \pid -> do
      flipFirstQuest pid
      objectives <- getsGame (sort . map toCardCode . toList . (.entities.objectives))
      objectives `shouldBe` sort (map toCardCode [Objectives.theGoblinsAssault, Objectives.theHeroesDefense])
      enemies <- getsGame (map toCardCode . toList . (.entities.enemies))
      enemies `shouldSatisfy` elem (toCardCode Enemies.urdug)
      length enemies `shouldBe` 2
      activeQuestCode `shouldReturn` toCardCode Quests.urdugsAssault
      quests <- getsGame (map toCardCode . scenarioQuestDeck . toAttrs . (.scenario))
      quests `shouldBe` map toCardCode [Quests.outflanked, Quests.theFinalPush]
      deck <- getEncounterDeck
      filter ((`elem` map toCardCode [Objectives.theGoblinsAssault, Objectives.theHeroesDefense, Enemies.urdug]) . toCardCode) deck `shouldBe` []

  describe "Urdug and the objectives" do
    it "gives Urdug +2 defense for each other enemy, and no damage until he has attacked this phase" . fateTest $ \_ -> do
      urdug <- board
      getEnemyDefense urdug `shouldReturn` 2
      _ <- stage Enemies.urdugsElite
      _ <- stage Enemies.fierceVanguard
      getEnemyDefense urdug `shouldReturn` 6
      mods <- getModifiers urdug
      (ImmuneToPlayerCardEffects `elem` mods, CannotTakeDamage `elem` mods) `shouldBe` (True, True)
      run $ CreateEffect $ Effect GameSource (EnemyTarget urdug) [AttackedThisPhase] UntilEndOfPhase
      getModifiers urdug >>= (`shouldSatisfy` notElem CannotTakeDamage)

    it "has The Goblins' Assault take 1 progress per player as the players quest unsuccessfully" . fateTest $ \_ -> do
      _ <- board
      run $ CheckWindows [mkAfter QuestedUnsuccessfully]
      assaultProgress `shouldReturn` 1

    it "has The Heroes' Defense take a resource after an enemy is destroyed, and engage Urdug with each player at 5 per player" . fateTest $ \pid -> do
      urdug <- board
      elite <- stage Enemies.urdugsElite
      run $ Destroy GameSource (EnemyTarget elite)
      chooseAbilityOf pid =<< defense
      defenseResources `shouldReturn` 1
      getModifiers urdug >>= (`shouldSatisfy` notElem (EngagedWithEachPlayer AnyPlayer))
      d <- defense
      run $ PlaceTokens GameSource d Resource 4
      getModifiers urdug >>= (`shouldSatisfy` elem (EngagedWithEachPlayer AnyPlayer))

    it "has Urdug attack each player in the combat phase once he is considered engaged" . fateTest $ \pid -> do
      urdug <- board
      d <- defense
      run $ PlaceTokens GameSource d Resource 5
      _ <- setEncounterDeck [Locations.hilltopBattlements, Locations.dwarvenWatchtower]
      run $ DealShadowCard urdug
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      Just (QuestionLabel _ source _ _) <- getQuestion pid
      source `shouldBe` Just (EnemyTarget urdug)

    it "takes no resource on The Heroes' Defense while Gate of Gundabad is in the staging area" . fateTest $ \pid -> do
      _ <- board
      _ <- stageLocation Locations.gateOfGundabad
      d <- defense
      run $ PlaceTokens GameSource d Resource 2
      defenseResources `shouldReturn` 0
      elite <- stage Enemies.urdugsElite
      run $ Destroy GameSource (EnemyTarget elite)
      offersAbilityOf pid d `shouldReturn` False

    it "has Urdug discard the top encounter card as he attacks, gaining its threat in attack" . fateTest $ \pid -> do
      urdug <- board
      _ <- setEncounterDeck [Locations.hilltopBattlements]
      run $ CheckWindows [mkWhen $ EnemyAttacking urdug pid]
      encounterDiscardCodes `shouldReturn` [toCardCode Locations.hilltopBattlements]
      getEnemyAttack urdug `shouldReturn` 6

    it "is won as Urdug is destroyed" . fateTest $ \_ -> do
      urdug <- board
      run $ Destroy GameSource (EnemyTarget urdug)
      getsGame (.outcome) `shouldReturn` Just Won

  describe "stages" do
    it "has stage 1B place progress for each enemy in the staging area, then advance at 2 plus 2 per player" . fateTest $ \pid -> do
      _ <- board
      flippedStage pid Quests.urdugsAssault
      setQuestDeck [Quests.outflanked]
      _ <- setEncounterDeck []
      _ <- stage Enemies.urdugsElite
      run $ CheckWindows [mkAfter RoundEnds]
      assaultProgress `shouldReturn` 2
      activeQuestCode `shouldReturn` toCardCode Quests.urdugsAssault
      run $ CheckWindows [mkAfter RoundEnds]
      assaultProgress `shouldReturn` 4
      activeQuestCode `shouldReturn` toCardCode Quests.outflanked
      Just (QuestionLabel l _ _ _) <- getQuestion pid
      l `shouldSatisfy` ("$label.resolveWhenRevealed" `isPrefixOf`)

    it "has stage 2A shuffle the discard pile in and add discarded enemies, one per player" . fateTest $ \pid -> do
      _ <- board
      q <- putQuestStage Quests.outflanked
      _ <- setEncounterDeck [Locations.hilltopBattlements, Enemies.urdugsElite]
      run $ Flip q
      resolveWhenRevealed pid
      getsGame (any ((== toCardCode Enemies.urdugsElite) . toCardCode) . (.entities.enemies)) `shouldReturn` True

    it "has stage 2B place progress after an enemy attacks, and advance at 4 plus 4 per player" . fateTest $ \pid -> do
      urdug <- board
      flippedStage pid Quests.outflanked
      setQuestDeck [Quests.theFinalPush]
      run $ CheckWindows [mkAfter $ EnemyAttacking urdug pid]
      assaultProgress `shouldReturn` 1
      a <- assault
      run $ PlaceTokens GameSource a Progress 7
      run $ CheckWindows [mkAfter RoundEnds]
      activeQuestCode `shouldReturn` toCardCode Quests.theFinalPush

    it "has stage 3B place progress for each enemy in play, and lose at 8 plus 8 per player" . fateTest $ \pid -> do
      _ <- board
      flippedStage pid Quests.theFinalPush
      elite <- stage Enemies.urdugsElite
      run $ EngageEnemy pid elite
      chooseLabel "$label.theFateOfWilderland.placeProgress" pid
      a <- assault
      run $ PlaceTokens GameSource a Progress 12
      run $ CheckWindows [mkAfter RoundEnds]
      assaultProgress `shouldReturn` 15
      getsGame (.outcome) `shouldReturn` Nothing
      run $ CheckWindows [mkAfter RoundEnds]
      getsGame (.outcome) `shouldReturn` Just Lost

  describe "encounter cards" do
    it "has Urdug's Elite exhaust a character or place progress as it engages" . fateTest $ \pid -> do
      _ <- board
      elite <- stage Enemies.urdugsElite
      run $ EngageEnemy pid elite
      chooseTarget pid =<< hero "01012"
      heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
      assaultProgress `shouldReturn` 0

    it "has Fierce Vanguard gain +2 attack this round as it engages, if chosen" . fateTest $ \pid -> do
      _ <- board
      vanguard <- stage Enemies.fierceVanguard
      run $ EngageEnemy pid vanguard
      chooseLabel "$label.fierceVanguard.attack" pid
      getEnemyAttack vanguard `shouldReturn` 4

    it "has Warg-rider return to the staging area when dealt a shadow card with no effect" . fateTest $ \pid -> do
      _ <- board
      rider <- stage Enemies.wargRider
      run $ EngageEnemy pid rider
      _ <- setEncounterDeck [Locations.hilltopBattlements]
      run $ DealShadowCard rider
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      chooseTarget pid =<< hero "01012"
      passActions pid
      resolveShadowsInOrder pid
      chooseLabel "$label.wargRider.return" pid
      getsGame (fmap (.placement) . lookup rider . (.entities.enemies)) `shouldReturn` Just StagingArea

    it "has Gate of Gundabad fetch a Goblin Troop as the players travel there" . fateTest $ \_ -> do
      _ <- board
      _ <- setEncounterDeck [Locations.hilltopBattlements, Enemies.goblinTroop]
      gate <- stageLocation Locations.gateOfGundabad
      getModifiers (LocationTarget gate) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ TravelTo gate
      getsGame (any ((== toCardCode Enemies.goblinTroop) . toCardCode) . (.entities.enemies)) `shouldReturn` True

    it "has Hilltop Battlements lower engagement costs, and add discarded enemies as it is explored" . fateTest $ \_ -> do
      urdug <- board
      elite <- stage Enemies.urdugsElite
      lid <- stageLocation Locations.hilltopBattlements
      getEngagementCost elite `shouldReturn` 20
      getEngagementCost urdug `shouldReturn` 50
      _ <- setEncounterDeck [Enemies.fierceVanguard]
      run $ CheckWindows [mkWhen $ Explored lid]
      getsGame (any ((== toCardCode Enemies.fierceVanguard) . toCardCode) . (.entities.enemies)) `shouldReturn` True

    it "gives each hero +1 defense while Dwarven Watchtower is active, and lets the first player choose after it is explored" . fateTest $ \pid -> do
      _ <- board
      lid <- stageLocation Locations.dwarvenWatchtower
      h <- hero "01012"
      printed <- getDefense h
      run $ PlaceLocation lid ActiveLocation
      getDefense h `shouldReturn` (printed + 1)
      a <- assault
      run $ PlaceTokens GameSource a Progress 2
      run $ CheckWindows [mkAfter $ Explored lid]
      chooseAbilityOf pid (LocationTarget lid)
      chooseLabel "$label.dwarvenWatchtower.progress" pid
      assaultProgress `shouldReturn` 1

    it "has Slopes of Gundabad place progress on The Goblins' Assault, if chosen" . fateTest $ \pid -> do
      _ <- board
      reveal [Locations.slopesOfGundabad]
      chooseLabel "$label.theFateOfWilderland.placeProgress" pid
      assaultProgress `shouldReturn` 1

    it "has Fierce Attack place progress equal to the stage number" . fateTest $ \_ -> do
      _ <- board
      _ <- putQuestStage Quests.outflanked
      reveal [Treacheries.fierceAttack]
      assaultProgress `shouldReturn` 2

    it "has Urdug's Command make Urdug attack the first player" . fateTest $ \pid -> do
      urdug <- board
      reveal [Treacheries.urdugsCommand, Locations.hilltopBattlements, Locations.dwarvenWatchtower]
      passActions pid
      Just (QuestionLabel _ source _ _) <- getQuestion pid
      source `shouldBe` Just (EnemyTarget urdug)

    it "has Dark Clouds raise staged threat and place progress as the players quest unsuccessfully" . fateTest $ \_ -> do
      urdug <- board
      reveal [Treacheries.darkClouds]
      getEnemyThreat urdug `shouldReturn` 3
      inPhase QuestPhase
      run QuestResolution
      -- 1 from Dark Clouds, 1 from The Goblins' Assault's own Forced effect
      assaultProgress `shouldReturn` 2
