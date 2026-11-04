module LordOfTheRings.HeirsOfNumenor.NightmareSpec (spec) where

import Data.Map.Strict qualified as Map

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (intoIthilien, perilInPelargir, theSiegeOfCairAndros)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Types (GameMode (..), modeL, scenarioSetAside, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemiesOf :: CardDef -> GameT [EnemyId]
enemiesOf def = getsGame (map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = fromJustNote "no such enemy" . headMay <$> enemiesOf def

locationOf :: CardDef -> GameT (Maybe LocationId)
locationOf def = getsGame (headMay . map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  fromJustNote "no such location" <$> locationOf def

scrollOn :: Target -> GameT ()
scrollOn host = run . (`PutObjectiveIntoPlayAttached` host) =<< genEncounterCard Objectives.alcaronsScroll

scrollPlacement :: GameT (Maybe Placement)
scrollPlacement = getsGame (headMay . map (.placement) . toList . (.entities.objectives))

progressOn :: EnemyId -> GameT Int
progressOn eid = getsGame (maybe 0 (countTokens Progress . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))

io :: IO () -> IO ()
io = id

spec :: Spec
spec = describe "Heirs of Númenor nightmare decks" do
  describe "Peril in Pelargir" do
    it "builds the nightmare deck: removals out, the nightmare set in" . nightmare perilInPelargir $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 43
      map (`copies` deck) [Enemies.harborThug, Enemies.pickpocket, Treacheries.collateralDamage, Enemies.cunningInformant]
        `shouldBe` [4, 0, 0, 3]
      copies Nightmare.perilInPelargirNightmare deck `shouldBe` 0

    it "loses the game once an enemy has 3 progress" . nightmare perilInPelargir $ \_ -> do
      eid <- staged Enemies.harborBrute
      run $ PlaceTokens GameSource (toTarget eid) Progress 3
      getsGame (.outcome) `shouldReturn` Just Lost

    it "places progress on the scroll's enemy as the refresh phase begins" . nightmare perilInPelargir $ \_ -> do
      eid <- staged Enemies.harborBrute
      scrollOn (toTarget eid)
      run $ CheckWindows [mkAfter $ PhaseBegins RefreshPhase]
      progressOn eid `shouldReturn` 1

    it "attaches the scroll in the staging area to the highest defense enemy as combat ends" . nightmare perilInPelargir $ \_ -> do
      brute <- staged Enemies.harborBrute
      _ <- staged Enemies.cunningInformant
      scrollOn (toTarget brute)
      oid <- getsGame (fromJustNote "no scroll" . fmap toId . headMay . toList . (.entities.objectives))
      run $ PlaceObjective oid StagingArea
      run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
      scrollPlacement `shouldReturn` Just (AttachedTo $ toTarget brute)

    it "Harbor Brute takes the scroll and its progress" . nightmare perilInPelargir $ \_ -> do
      informant <- staged Enemies.cunningInformant
      scrollOn (toTarget informant)
      run $ PlaceTokens GameSource (toTarget informant) Progress 2
      reveal Enemies.harborBrute []
      brute <- enemyOf Enemies.harborBrute
      scrollPlacement `shouldReturn` Just (AttachedTo $ toTarget brute)
      progressOn brute `shouldReturn` 2
      progressOn informant `shouldReturn` 0

    it "The Thieves' Ploy moves the scroll to an engaged enemy with 1 progress" . nightmare perilInPelargir $ \pid -> do
      scrollOn . toTarget =<< getHeroWithCode "01012"
      informant <- staged Enemies.cunningInformant
      run $ EngageEnemy pid informant
      reveal Treacheries.theThievesPloy [Locations.pelargirAlley]
      scrollPlacement `shouldReturn` Just (AttachedTo $ toTarget informant)
      progressOn informant `shouldReturn` 1

    it "Cunning Informant gets +3 attack while the scroll is on a hero" . nightmare perilInPelargir $ \_ -> do
      informant <- staged Enemies.cunningInformant
      getEnemyAttack informant `shouldReturn` 2
      scrollOn . toTarget =<< getHeroWithCode "01012"
      getEnemyAttack informant `shouldReturn` 5

    it "Dockside Street keeps enemies in the staging area from damage" . nightmare perilInPelargir $ \_ -> do
      _ <- stagedLocation Locations.docksideStreet
      brute <- staged Enemies.harborBrute
      run $ DealDamage GameSource (toTarget brute) 2
      getDamage (toTarget brute) `shouldReturn` 0

    it "Pelargir Alley gets +3 threat while the scroll is not on a hero" . nightmare perilInPelargir $ \_ -> do
      _ <- stagedLocation Locations.pelargirAlley
      getStagingThreat `shouldReturn` 5
      scrollOn . toTarget =<< getHeroWithCode "01012"
      getStagingThreat `shouldReturn` 2

  describe "Into Ithilien" do
    it "builds the nightmare deck, the Mumak Elites set aside" . nightmare intoIthilien $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 41
      map (`copies` deck) [Locations.secludedGlade, Enemies.forestBat, Enemies.southronMercenaries, Enemies.mumakElite, Enemies.haradrimMarksman]
        `shouldBe` [0, 0, 0, 0, 3]
      setAside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      copies Enemies.mumakElite setAside `shouldBe` 2

    it "adds a Mumak Elite to the staging area as stage 2 is revealed" . nightmare intoIthilien $ \_ -> do
      run $ DoStep 1 Setup
      _ <- putQuestStage Quests.southronCounterAttack
      length <$> enemiesOf Enemies.mumakElite `shouldReturn` 1
      setAside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      copies Enemies.mumakElite setAside `shouldBe` 1

    it "Mumak Elite has archery equal to the players and takes at most 3 damage a round" . nightmare intoIthilien $ \_ -> do
      eid <- staged Enemies.mumakElite
      getArcheryTotal `shouldReturn` 1
      run $ DealDamage GameSource (toTarget eid) 5
      getDamage (toTarget eid) `shouldReturn` 3

    it "Haradrim Marksman gets +2 attack and no ally defenders under siege" . nightmare intoIthilien $ \_ -> do
      _ <- putQuestStage Quests.southronCounterAttack
      eid <- staged Enemies.haradrimMarksman
      getEnemyAttack eid `shouldReturn` 4
      getModifiers eid >>= (`shouldSatisfy` elem CannotBeDefendedByAllies)

    it "Ithilien Overlook keeps archery damage on the character first assigned it" . nightmare intoIthilien $ \pid -> do
      _ <- stagedLocation Locations.ithilienOverlook
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ ArcheryDamage 2 []
      chooseTarget pid eowyn
      question <- fromJustNote "no question" <$> getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldBe` [eowyn]
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 2

    it "Obsidian Arrows gives the quest archery 2 and fires it at once" . nightmare intoIthilien $ \pid -> do
      _ <- putQuestStage Quests.theHiddenWay
      eowyn <- toTarget <$> getHeroWithCode "01007"
      reveal Treacheries.obsidianArrows []
      chooseTarget pid eowyn
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 2

    it "Savage Fray switches siege to battle" . nightmare intoIthilien $ \_ -> do
      _ <- putQuestStage Quests.southronCounterAttack
      eid <- staged Enemies.haradrimMarksman
      reveal Treacheries.savageFray []
      -- under battle: +2 threat
      getStagingThreat `shouldReturn` 4
      getEnemyAttack eid `shouldReturn` 2

    it "Savage Fray surges with no keyword on the quest" . nightmare intoIthilien $ \_ -> do
      _ <- putQuestStage Quests.theHiddenWay
      reveal Treacheries.savageFray [Locations.cormallenFields]
      isJust <$> locationOf Locations.cormallenFields `shouldReturn` True

  describe "The Siege of Cair Andros" do
    it "builds the nightmare deck" . nightmare theSiegeOfCairAndros $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 48
      map (`copies` deck) [Enemies.orcArbalesters, Enemies.orcRabble, Enemies.southronMercenaries, Enemies.orcRavagerNightmare]
        `shouldBe` [0, 1, 0, 4]

    it "player cards in the victory display score nothing" $ io do
      card <- genPlayerCard Events.justiceShallBeDone Nothing
      let withCard m = toAttrs (overAttrs ((modeL .~ m) . (victoryDisplayL .~ [PlayerCard card])) theSiegeOfCairAndros)
      (withCard NightmareMode).victoryPoints `shouldBe` 0
      (withCard NormalMode).victoryPoints `shouldBe` 1

    it "Besieged Courtyard is discarded at 5 damage, exhausting each ready character" . nightmare theSiegeOfCairAndros $ \_ -> do
      lid <- stagedLocation Locations.besiegedCourtyard
      run $ DealDamage GameSource (toTarget lid) 5
      locationOf Locations.besiegedCourtyard `shouldReturn` Nothing
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True

    it "Anduin Wharf is discarded at 6 damage and reveals 2 cards" . nightmare theSiegeOfCairAndros $ \_ -> do
      lid <- stagedLocation Locations.anduinWharf
      _ <- setEncounterDeck [Locations.wallsOfCairAndros, Enemies.orcSaboteur]
      run $ DealDamage GameSource (toTarget lid) 6
      resolveRevealed
      locationOf Locations.anduinWharf `shouldReturn` Nothing
      isJust <$> locationOf Locations.wallsOfCairAndros `shouldReturn` True
      length <$> enemiesOf Enemies.orcSaboteur `shouldReturn` 1

    it "Orc Saboteur's undefended damage goes to a Battleground" . nightmare theSiegeOfCairAndros $ \pid -> do
      lid <- stagedLocation Locations.anduinWharf
      eid <- staged Enemies.orcSaboteur
      run $ EngageEnemy pid eid
      run $ UndefendedDamage pid eid
      getDamage (toTarget lid) `shouldReturn` 5

    it "Orc Saboteur's attack damage goes to a Battleground, other damage to the defender" . nightmare theSiegeOfCairAndros $ \pid -> do
      lid <- stagedLocation Locations.anduinWharf
      eid <- staged Enemies.orcSaboteur
      run $ EngageEnemy pid eid
      beravor <- toTarget <$> getHeroWithCode "01012"
      run $ CheckWindows [mkAfter (DeclaredAsDefender beravor eid)]
      run $ DealDamage (EnemySource eid) beravor 1
      getDamage beravor `shouldReturn` 1
      run $ AttackDamage eid beravor
      getDamage (toTarget lid) `shouldReturn` 3
      getDamage beravor `shouldReturn` 1

    it "Southron Mounted Archer lets archery damage go to a Battleground" . nightmare theSiegeOfCairAndros $ \pid -> do
      lid <- stagedLocation Locations.anduinWharf
      _ <- staged Enemies.southronMountedArcher
      run $ ArcheryDamage 1 []
      chooseTarget pid lid
      getDamage (toTarget lid) `shouldReturn` 1

    it "Ruin and Wroth deals 2 shadow cards to each enemy and 2 damage to each Battleground" . nightmare theSiegeOfCairAndros $ \_ -> do
      lid <- stagedLocation Locations.anduinWharf
      eid <- staged Enemies.orcSaboteur
      reveal Treacheries.ruinAndWroth [Enemies.orcRavagerNightmare, Enemies.orcRavagerNightmare, Enemies.orcSaboteur]
      length . (.shadowCards) <$> getEnemy eid `shouldReturn` 2
      getDamage (toTarget lid) `shouldReturn` 2

    it "Orc Ravager gets +2 defense when dealt a shadow card" . nightmare theSiegeOfCairAndros $ \_ -> do
      eid <- staged Enemies.orcRavagerNightmare
      _ <- setEncounterDeck [Enemies.orcSaboteur]
      run $ DealShadowCard eid
      getEnemyDefense eid `shouldReturn` 3
