module LordOfTheRings.AgainstTheShadow.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theBloodOfGondor, theMorgulVale)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (towerProgress)
import LordOfTheRings.Scenario.Types (GameMode (..), modeL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (RoundEnds))
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

endRound :: GameT ()
endRound = run $ CheckWindows [Window When RoundEnds]

-- | To the Tower, in the staging area.
tower :: GameT ObjectiveId
tower = do
  run . AddToStaging =<< genEncounterCard Objectives.toTheTower
  getsGame (fromJustNote "no tower" . fmap toId . headMay . toList . (.entities.objectives))

hiddenOf :: PlayerId -> GameT [CardCode]
hiddenOf pid = getsGame (map toCardCode . findWithDefault [] pid . (.hiddenCards) . toAttrs . (.scenario))

-- | The player takes the cards as hidden cards, in order.
hide :: PlayerId -> [CardDef] -> GameT ()
hide pid defs = do
  _ <- setEncounterDeck defs
  replicateM_ (length defs) $ run (TakeHiddenCard pid)

spec :: Spec
spec = describe "Against the Shadow nightmare decks" do
  describe "The Morgul Vale" do
    it "builds the nightmare deck" . nightmare theMorgulVale $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.morgulRoad, Enemies.forestBat, Enemies.morgulTracker, Enemies.warriorOfNurn, Locations.ephelDuath]
        `shouldBe` [0, 0, 1, 3, 3]
      copies Nightmare.theMorgulValeNightmare deck `shouldBe` 0

    it "places 1 progress on To the Tower at setup" . nightmare theMorgulVale $ \pid -> do
      flipFirstQuest pid
      towerProgress `shouldReturn` 1

    it "Sentry of the White Bridge's attack and threat are the tower's progress" . nightmare theMorgulVale $ \_ -> do
      oid <- tower
      run $ PlaceTokens GameSource (toTarget oid) Progress 2
      eid <- staged Enemies.sentryOfTheWhiteBridge
      getEnemyAttack eid `shouldReturn` 2

    it "Warrior of Nurn shields and heals the Captains" . nightmare theMorgulVale $ \_ -> do
      murzag <- staged Enemies.murzag
      _ <- staged Enemies.warriorOfNurn
      getEnemyDefense murzag `shouldReturn` 3
      run $ DealDamage GameSource (toTarget murzag) 2
      endRound
      (.damage) <$> getEnemy murzag `shouldReturn` 0

    it "Orc Captor in the staging area places progress at the end of the round" . nightmare theMorgulVale $ \_ -> do
      _ <- tower
      _ <- staged Enemies.orcCaptor
      endRound
      towerProgress `shouldReturn` 1

    it "Ephel Dúath in the staging area gives Captains +2 attack" . nightmare theMorgulVale $ \_ -> do
      murzag <- staged Enemies.murzag
      reveal Locations.ephelDuath []
      getEnemyAttack murzag `shouldReturn` 7

    it "White-Flowered Fields: each player exhausts a hero or places progress" . nightmare theMorgulVale $ \pid -> do
      _ <- tower
      reveal Locations.whiteFloweredFields []
      endRound
      chooseLabel "$label.whiteFloweredFields.progress" pid
      towerProgress `shouldReturn` 1

    it "The Tower Beckons places progress and adds the tower's progress to the staging area" . nightmare theMorgulVale $ \_ -> do
      _ <- tower
      reveal Treacheries.theTowerBeckons []
      towerProgress `shouldReturn` 1
      getStagingThreat `shouldReturn` 1

    it "Noxious Fumes deals damage equal to the tower's progress" . nightmare theMorgulVale $ \pid -> do
      _ <- tower
      reveal Treacheries.noxiousFumes []
      towerProgress `shouldReturn` 1
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 1

    it "Banks of Morgulduin's shadow deals a Captain 3 more shadow cards" . nightmare theMorgulVale $ \pid -> do
      murzag <- staged Enemies.murzag
      run $ EngageEnemy pid murzag
      _ <- setEncounterDeck [Treacheries.impenetrableFog, Treacheries.impenetrableFog, Treacheries.impenetrableFog, Locations.morgulVale]
      shadow pid murzag Locations.banksOfMorgulduin []
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.morgulVale]

    it "Orc Captor's shadow gives +3 defense" . nightmare theMorgulVale $ \pid -> do
      murzag <- staged Enemies.murzag
      shadow pid murzag Enemies.orcCaptor []
      getEnemyDefense murzag `shouldReturn` 5

  describe "The Blood of Gondor" do
    it "builds the nightmare deck, the nightmare Black Númenórean in place of the standard one" . nightmare theBloodOfGondor $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.blackNumenorean, Locations.theDarkWoods, Enemies.blackNumenoreanNightmare, Enemies.orcTrickster]
        `shouldBe` [0, 0, 1, 4]

    it "stages the nightmare Black Númenórean and gives each player a hidden card at setup" . nightmare theBloodOfGondor $ \pid -> do
      flipFirstQuest pid
      length <$> hiddenOf pid `shouldReturn` 1
      bn <- enemyOf Enemies.blackNumenoreanNightmare
      (.placement) <$> getEnemy bn `shouldReturn` StagingArea

    it "cannot be won until The Cross-roads and Black Númenórean are in the victory display" . nightmare theBloodOfGondor $ \_ -> do
      quest <- putQuestStage Quests.captured
      run $ PlaceTokens GameSource quest Progress 15
      getsGame (.outcome) `shouldReturn` Nothing
      cards <- traverse (fmap EncounterCard . genEncounterCard) [Locations.theCrossroads, Enemies.blackNumenoreanNightmare]
      putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (cards <>))) =<< getGame
      run $ CheckWindows []
      getsGame (.outcome) `shouldReturn` Just Won

    it "Black Númenórean turns the hidden cards of the player it engages faceup" . nightmare theBloodOfGondor $ \pid -> do
      bn <- staged Enemies.blackNumenoreanNightmare
      hide pid [Locations.edgeOfIthilien, Locations.edgeOfIthilien]
      getEnemyThreat bn `shouldReturn` 2
      run $ EngageEnemy pid bn
      hiddenOf pid `shouldReturn` []

    it "Uruk Marksman turned faceup deals 3 damage and returns to the staging area" . nightmare theBloodOfGondor $ \pid -> do
      bn <- staged Enemies.blackNumenoreanNightmare
      hide pid [Enemies.urukMarksman]
      run $ EngageEnemy pid bn
      beravor <- toTarget <$> getHeroWithCode "01002"
      chooseTarget pid beravor
      getCharacterDamage beravor `shouldReturn` 3
      uruk <- enemyOf Enemies.urukMarksman
      (.placement) <$> getEnemy uruk `shouldReturn` StagingArea

    it "Orc Trickster damaged becomes a hidden card instead" . nightmare theBloodOfGondor $ \pid -> do
      trickster <- staged Enemies.orcTrickster
      run $ DealDamage GameSource (toTarget trickster) 1
      hiddenOf pid `shouldReturn` [toCardCode Enemies.orcTrickster]
      getsGame (length . (.entities.enemies)) `shouldReturn` 0

    it "Edge of Ithilien's archery is the most hidden cards any player has" . nightmare theBloodOfGondor $ \pid -> do
      reveal Locations.edgeOfIthilien []
      hide pid [Locations.mountainsOfShadow, Locations.mountainsOfShadow]
      getArcheryTotal `shouldReturn` 2

    it "Sudden Onslaught turns the hidden cards faceup and takes as many again" . nightmare theBloodOfGondor $ \pid -> do
      hide pid [Locations.mountainsOfShadow, Locations.edgeOfIthilien]
      reveal Treacheries.suddenOnslaught [Locations.theDarkWoods, Locations.theDarkWoods]
      hiddenOf pid `shouldReturn` map toCardCode [Locations.theDarkWoods, Locations.theDarkWoods]

    it "Mountains of Shadow active takes a hidden card for each turned faceup" . nightmare theBloodOfGondor $ \pid -> do
      reveal Locations.mountainsOfShadow []
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ PlaceLocation lid ActiveLocation
      hide pid [Locations.edgeOfIthilien]
      bn <- staged Enemies.blackNumenoreanNightmare
      _ <- setEncounterDeck [Locations.theDarkWoods]
      run $ EngageEnemy pid bn
      hiddenOf pid `shouldReturn` [toCardCode Locations.theDarkWoods]

    it "Surrounded by Shadow takes a hidden card for each questing hero" . nightmare theBloodOfGondor $ \pid -> do
      run . CommitToQuest . toTarget =<< getHeroWithCode "01007"
      reveal Treacheries.surroundedByShadow [Locations.theDarkWoods]
      chooseLabel "$label.surroundedByShadow.take" pid
      hiddenOf pid `shouldReturn` [toCardCode Locations.theDarkWoods]
