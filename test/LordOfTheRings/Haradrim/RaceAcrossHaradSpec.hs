module LordOfTheRings.Haradrim.RaceAcrossHaradSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (..))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, raceAcrossHarad)
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (Meta (..), getOrcsMeta)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), metaL, questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (CharacterDestroyedByAttack, PhaseEnds, StagingStepBegins), mkAfter, mkWhen)
import TestImport

raceTest :: (PlayerId -> GameT ()) -> IO ()
raceTest = gameTestIn raceAcrossHarad testDeck

refresh :: Game -> GameT ()
refresh g = putGame g >> (putGame =<< preloadModifiers g)

-- | The Orcs' area prepared, a Tamed Mumak under the player and Uruk Chieftain in the area.
prepare :: GameT EnemyId
prepare = do
  cards <- traverse genEncounterCard [Objectives.tamedMumak, Enemies.urukChieftain]
  getGame >>= putGame . (scenarioL %~ overAttrs (setAsideL .~ cards))
  run $ ScenarioSpecific "orcs.prepare" Null
  enemyOf Enemies.urukChieftain

-- | The Orcs at the given stage, the stages after it beneath, with this much progress.
orcsAt :: CardDef -> [CardDef] -> Int -> GameT ()
orcsAt def rest progress = do
  top <- genEncounterCard def
  deck <- traverse genEncounterCard rest
  meta <- getOrcsMeta
  refresh . (scenarioL %~ overAttrs (metaL .~ toJSON meta {orcsStage = Just top, orcsDeck = deck, orcsProgress = progress})) =<< getGame

-- | The players' stage in play on its B side, the stages given beneath it.
playersAt :: CardDef -> [CardDef] -> GameT QuestId
playersAt def rest = do
  QuestTarget q <- putQuestStage def
  deck <- traverse genEncounterCard rest
  refresh . (scenarioL %~ overAttrs (questDeckL .~ deck)) . (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) =<< getGame
  pure q

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOf :: EnemyId -> GameT Placement
placementOf eid = getsGame (maybe Unplaced (.placement) . lookup eid . (.entities.enemies))

locationPlacement :: LocationId -> GameT Placement
locationPlacement lid = getsGame (maybe Unplaced (.placement) . lookup lid . (.entities.locations))

theOrcsStage :: GameT (Maybe CardCode)
theOrcsStage = fmap toCardCode . (.orcsStage) <$> getOrcsMeta

theOrcsProgress :: GameT Int
theOrcsProgress = (.orcsProgress) <$> getOrcsMeta

endQuestPhase :: GameT ()
endQuestPhase = run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

-- | Answer the searches of the encounter deck, each with its first card.
answerSearches :: PlayerId -> GameT ()
answerSearches pid =
  getQuestion pid >>= \case
    Just (QuestionLabel _ _ _ (SearchCards _ _)) -> chooseOptionMatching "search" pid (const True) >> answerSearches pid
    Just (SearchCards _ _) -> chooseOptionMatching "search" pid (const True) >> answerSearches pid
    _ -> pure ()

mumakOf :: GameT AllyId
mumakOf = getsGame (fromJustNote "no Tamed Mumak" . fmap toId . find ((== toCardCode Objectives.tamedMumak) . toCardCode) . toList . (.entities.allies))

spec :: Spec
spec = describe "Race Across Harad" do
  it "is a scenario the game can be dealt" . raceTest $ \_ ->
    isJust (lookupScenario "race-across-harad") `shouldBe` True

  it "Setup gives the player a Tamed Mumak, prepares the Orcs' area, stages a location and advances to stage 2" . raceTest $ \pid -> do
    flipFirstQuest pid
    answerSearches pid
    mumak <- mumakOf
    getsGame (fmap (.placement) . lookup mumak . (.entities.allies)) `shouldReturn` Just (InPlayArea pid)
    chieftain <- enemyOf Enemies.urukChieftain
    placementOf chieftain `shouldReturn` OrcsArea
    theOrcsStage `shouldReturn` Just (toCardCode Quests.settingOutOrcs)
    map toCardCode . (.orcsDeck) <$> getOrcsMeta
      `shouldReturn` map toCardCode [Quests.racingNorthOrcs, Quests.huntedInHaradOrcs, Quests.theFordOfHarnenOrcs]
    getsGame (map toCardCode . toList . (.entities.quests)) `shouldReturn` [toCardCode Quests.racingNorth]
    setAside <- getsGame (map toCardCode . (.scenarioSetAside) . toAttrs . (.scenario))
    setAside `shouldSatisfy` \cs -> sort cs == sort [toCardCode Enemies.urukWargRider, toCardCode Locations.riverHarnen]
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Objectives.tamedMumak)
    -- Racing North: the set aside Uruk Warg-rider to the Orcs' area
    resolveRevealed
    chooseLabel "$label.racingNorth.orcsArea" pid
    rider <- enemyOf Enemies.urukWargRider
    placementOf rider `shouldReturn` OrcsArea

  describe "The Orcs' area" do
    it "keeps its cards out of the staging area, immune, and in the area" . raceTest $ \pid -> do
      chieftain <- prepare
      _ <- playersAt Quests.racingNorth []
      getModifiers (EnemyTarget chieftain) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getStagingThreat `shouldReturn` 0
      run $ EngageEnemy pid chieftain
      placementOf chieftain `shouldReturn` OrcsArea
      -- an encounter card's "return that enemy to the staging area" leaves it there too
      run $ PlaceEnemy chieftain StagingArea
      placementOf chieftain `shouldReturn` OrcsArea

    it "has the Orcs place the discarded card's threat plus their area's at the end of the quest phase" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.racingNorth []
      _ <- setEncounterDeck [Locations.expanseOfHarad]
      endQuestPhase
      -- Uruk Chieftain's 4 and Expanse of Harad's 2
      theOrcsProgress `shouldReturn` 6

    it "puts the Orcs' area in the staging area once the Orcs reach the players' stage" . raceTest $ \pid -> do
      chieftain <- prepare
      _ <- playersAt Quests.racingNorth []
      orcsAt Quests.settingOutOrcs [Quests.racingNorthOrcs, Quests.huntedInHaradOrcs] 8
      _ <- setEncounterDeck [Locations.expanseOfHarad]
      endQuestPhase
      theOrcsStage `shouldReturn` Just (toCardCode Quests.racingNorthOrcs)
      placementOf chieftain `shouldReturn` StagingArea
      getStagingThreat `shouldReturn` 4
      getModifiers (EnemyTarget chieftain) >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)
      -- engaged, it returns to the Orcs' area, the staging area now, at the end of the quest phase
      run $ EngageEnemy pid chieftain
      placementOf chieftain `shouldReturn` EngagedWith pid
      _ <- setEncounterDeck [Locations.expanseOfHarad]
      endQuestPhase
      placementOf chieftain `shouldReturn` StagingArea

    it "takes its cards back from the staging area as the players move on" . raceTest $ \_ -> do
      chieftain <- prepare
      q <- playersAt Quests.racingNorth [Quests.huntedInHarad, Quests.theFordOfHarnen]
      orcsAt Quests.settingOutOrcs [Quests.racingNorthOrcs, Quests.huntedInHaradOrcs] 9
      _ <- setEncounterDeck [Locations.expanseOfHarad]
      endQuestPhase
      placementOf chieftain `shouldReturn` StagingArea
      _ <- setEncounterDeck [Locations.blisteringErg, Enemies.urukOfMordor]
      run $ CompleteQuest q
      resolveRevealed
      placementOf chieftain `shouldReturn` OrcsArea
      -- Hunted in Harad: the first Orc enemy discarded (the discard pile shuffled in) to
      -- the Orcs' area
      uruk <- enemyOf Enemies.urukOfMordor
      placementOf uruk `shouldReturn` OrcsArea

    it "Stage 2C adds an Orc enemy to the Orcs' area if the players are past stage 2B" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.huntedInHarad []
      orcsAt Quests.settingOutOrcs [Quests.racingNorthOrcs] 9
      _ <- setEncounterDeck [Locations.expanseOfHarad, Locations.blisteringErg, Enemies.urukOfMordor]
      endQuestPhase
      theOrcsStage `shouldReturn` Just (toCardCode Quests.racingNorthOrcs)
      uruk <- enemyOf Enemies.urukOfMordor
      placementOf uruk `shouldReturn` OrcsArea

    it "Stage 3D gives each enemy in the Orcs' area +1 threat while the players are at stage 4B" . raceTest $ \_ -> do
      chieftain <- prepare
      _ <- playersAt Quests.theFordOfHarnen []
      orcsAt Quests.huntedInHaradOrcs [Quests.theFordOfHarnenOrcs] 0
      getEnemyThreat chieftain `shouldReturn` 5

    it "Stage 4D counts the engaged enemies' threat, and Uruk Chieftain takes no damage while River Harnen is staged" . raceTest $ \pid -> do
      chieftain <- prepare
      _ <- playersAt Quests.theFordOfHarnen []
      orcsAt Quests.theFordOfHarnenOrcs [] 0
      run . AddToStaging =<< genEncounterCard Locations.riverHarnen
      getModifiers (EnemyTarget chieftain) >>= (`shouldSatisfy` elem CannotTakeDamage)
      orc <- stage Enemies.urukOfMordor
      run $ EngageEnemy pid orc
      _ <- setEncounterDeck [Locations.expanseOfHarad]
      endQuestPhase
      -- Uruk of Mordor's 2 and Expanse of Harad's 2: the Orcs' area does not count
      theOrcsProgress `shouldReturn` 4

  describe "Winning and losing" do
    it "the players lose as the Orcs defeat the stage the players are at" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.racingNorth []
      orcsAt Quests.racingNorthOrcs [Quests.huntedInHaradOrcs] 14
      _ <- setEncounterDeck [Locations.expanseOfHarad]
      endQuestPhase
      getsGame (.outcome) `shouldReturn` Just Lost

    it "the players lose as the Orcs defeat stage 4D" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.theFordOfHarnen []
      orcsAt Quests.theFordOfHarnenOrcs [] 18
      run $ ScenarioSpecific "orcs.progress" (toJSON (2 :: Int))
      getsGame (.outcome) `shouldReturn` Just Lost

    it "the players win defeating stage 4B, which River Harnen in play holds back" . raceTest $ \_ -> do
      _ <- prepare
      q <- playersAt Quests.theFordOfHarnen []
      run . AddToStaging =<< genEncounterCard Locations.riverHarnen
      river <- locationOf Locations.riverHarnen
      getModifiers (LocationTarget river) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ PlaceTokens GameSource (QuestTarget q) Progress 20
      getsGame (.outcome) `shouldReturn` Nothing
      getModifiers (LocationTarget river) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ AddToVictoryDisplay (LocationTarget river)
      run $ CheckStage q
      getsGame (.outcome) `shouldReturn` Just Won

  describe "The players' stages" do
    it "Racing North takes no more than 5 progress this round, and Uruk Chieftain takes no damage" . raceTest $ \_ -> do
      chieftain <- prepare
      q <- playersAt Quests.racingNorth []
      getModifiers (EnemyTarget chieftain) >>= (`shouldSatisfy` elem CannotTakeDamage)
      run $ CheckWindows [mkWhen StagingStepBegins]
      run $ PlaceTokens GameSource (QuestTarget q) Progress 12
      (.progress) <$> getActiveQuest `shouldReturn` 5

    it "Racing North takes 10 progress this round with each Tamed Mumak committed" . raceTest $ \_ -> do
      _ <- prepare
      q <- playersAt Quests.racingNorth []
      mumak <- mumakOf
      run $ CommitToQuest (AllyTarget mumak)
      run $ CheckWindows [mkWhen StagingStepBegins]
      run $ PlaceTokens GameSource (QuestTarget q) Progress 12
      (.progress) <$> getActiveQuest `shouldReturn` 10

    it "Hunted in Harad reveals an additional card while the Orcs are at stage 2D" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.huntedInHarad []
      orcsAt Quests.racingNorthOrcs [] 0
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))

    it "The Ford of Harnen places 1 progress on the Orcs' stage as an attack destroys a character" . raceTest $ \pid -> do
      _ <- prepare
      _ <- playersAt Quests.theFordOfHarnen []
      orc <- stage Enemies.urukOfMordor
      hero <- toTarget <$> getHeroWithCode "01012"
      run $ CheckWindows [mkAfter $ CharacterDestroyedByAttack orc hero pid]
      theOrcsProgress `shouldReturn` 1

  describe "The encounter cards" do
    it "Tamed Mumak takes undefended damage, and its controller is eliminated as it leaves play" . raceTest $ \pid -> do
      _ <- prepare
      mumak <- mumakOf
      orc <- stage Enemies.urukOfMordor
      run $ EngageEnemy pid orc
      run $ UndefendedDamage pid orc
      getDamage (AllyTarget mumak) `shouldReturn` 4
      run $ Discard (AllyTarget mumak)
      (.eliminated) <$> getPlayer pid `shouldReturn` True

    it "Harad Road and Uruk Pursuer go to the Orcs' area as they are revealed, the Pursuer surging" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.racingNorth []
      reveal [Locations.haradRoad]
      road <- locationOf Locations.haradRoad
      locationPlacement road `shouldReturn` OrcsArea
      _ <- setEncounterDeck [Enemies.urukPursuer, Locations.blisteringErg]
      run RevealEncounterCard
      resolveRevealed
      pursuer <- enemyOf Enemies.urukPursuer
      placementOf pursuer `shouldReturn` OrcsArea
      _ <- locationOf Locations.blisteringErg
      getLocationThreat road `shouldReturn` 5

    it "Expanse of Harad gets +2 threat while the players are not at the Orcs' stage" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.racingNorth []
      run . AddToStaging =<< genEncounterCard Locations.expanseOfHarad
      expanse <- locationOf Locations.expanseOfHarad
      getLocationThreat expanse `shouldReturn` 4

    it "Pursuers from Mordor adds the first Orc enemy discarded to the Orcs' area" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.racingNorth []
      reveal [Treacheries.pursuersFromMordor, Locations.blisteringErg, Enemies.urukOfMordor]
      uruk <- enemyOf Enemies.urukOfMordor
      placementOf uruk `shouldReturn` OrcsArea

    it "Howling at their Heels adds the Orcs' area's threat to the staging area's" . raceTest $ \_ -> do
      _ <- prepare
      _ <- playersAt Quests.racingNorth []
      reveal [Treacheries.howlingAtTheirHeels]
      getStagingThreat `shouldReturn` 4

    it "Racing Warg attaches to an Orc enemy, as a Mount" . raceTest $ \_ -> do
      orc <- stage Enemies.urukOfMordor
      threat <- getEnemyThreat orc
      _ <- setEncounterDeck [Enemies.racingWarg, Locations.blisteringErg]
      run RevealEncounterCard
      resolveRevealed
      warg <- enemyOf Enemies.racingWarg
      placementOf warg `shouldReturn` AttachedTo (EnemyTarget orc)
      getEnemyThreat orc `shouldReturn` threat + 1
