{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module LordOfTheRings.RingMaker.NightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (EncounterPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theDunlandTrap, theThreeTrials, troubleInTharbad)
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (eliminationLowering)
import LordOfTheRings.Scenario.Types (GameMode (..), modeL, scenarioMeta)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PhaseBegins))
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  staged def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

-- | Answer each question the player is asked with its first choice.
answerAll :: PlayerId -> GameT ()
answerAll pid =
  getQuestion pid >>= \case
    Just _ -> chooseOptionMatching "first choice" pid (const True) >> answerAll pid
    Nothing -> pure ()

heroDamage :: GameT Int
heroDamage = getsGame (sum . map (.damage) . toList . (.entities.heroes))

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

enemyCount :: GameT Int
enemyCount = getsGame (length . (.entities.enemies))

spec :: Spec
spec = describe "The Ring-maker Nightmare Decks" do
  describe "The Dunland Trap" do
    it "builds the nightmare deck" . nightmare theDunlandTrap $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.boarClanWarrior, Locations.hithaeglirStream, Treacheries.lowOnProvisions, Enemies.boarClanSkirmisher]
        `shouldBe` [0, 0, 2, 4]

    it "Boar Clan Skirmisher has the engaged player damage a hero as he draws" . nightmare theDunlandTrap $ \pid -> do
      _ <- engaged pid Enemies.boarClanSkirmisher
      run $ DrawCards $ Drawing pid 1
      answerAll pid
      heroDamage `shouldReturn` 1

    it "Plains of Dunland lowers Dunland enemies' engagement cost by 20" . nightmare theDunlandTrap $ \_ -> do
      staged Locations.plainsOfDunland
      staged Enemies.boarClanAmbusher
      (getEngagementCost =<< enemyOf Enemies.boarClanAmbusher) `shouldReturn` 10

    it "Hithaeglir Woods keeps damage off Dunland enemies in the staging area" . nightmare theDunlandTrap $ \_ -> do
      staged Locations.hithaeglirWoods
      staged Enemies.boarClanAmbusher
      eid <- enemyOf Enemies.boarClanAmbusher
      getModifiers eid >>= (`shouldSatisfy` elem CannotTakeDamage)

    it "Munuv Dûv River deals a shadow card to an engaged enemy as a player draws" . nightmare theDunlandTrap $ \pid -> do
      staged Locations.munuvDuvRiver
      eid <- engaged pid Enemies.boarClanAmbusher
      _ <- setEncounterDeck [Treacheries.boarClanAssault]
      run $ DrawCards $ Drawing pid 1
      answerAll pid
      length . (.shadowCards) <$> getEnemy eid `shouldReturn` 1

    it "Boar Clan Assault surges with no engaged enemy" . nightmare theDunlandTrap $ \_ -> do
      reveal Treacheries.boarClanAssault [Locations.plainsOfDunland]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "Stalked by Dunlendings adds the first player's hand size to the staging threat" . nightmare theDunlandTrap $ \pid -> do
      reveal Treacheries.stalkedByDunlendings []
      n <- handSize pid
      getStagingThreat `shouldReturn` n

    it "Stalked by Dunlendings' shadow engages the topmost enemy in the discard pile" . nightmare theDunlandTrap $ \pid -> do
      eid <- engaged pid Enemies.boarClanAmbusher
      run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.boarClanSkirmisher
      _ <- setEncounterDeck [Locations.plainsOfDunland]
      shadow pid eid Treacheries.stalkedByDunlendings []
      skirmisher <- enemyOf Enemies.boarClanSkirmisher
      length . (.shadowCards) <$> getEnemy skirmisher `shouldReturn` 1

  describe "The Three Trials" do
    it "builds the nightmare deck" . nightmare theThreeTrials $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.spiritOfTheWild, Locations.grimFoothills, Locations.tangledWoods, Treacheries.turnedAround, Enemies.wolfSpirit]
        `shouldBe` [2, 0, 2, 0, 3]

    it "adds each engaged Guardian's threat to the staging area" . nightmare theThreeTrials $ \pid -> do
      _ <- engaged pid Enemies.ravensGuardian
      getStagingThreat `shouldReturn` 3

    it "keeps damage off Guardians during the Trial of Intuition" . nightmare theThreeTrials $ \_ -> do
      _ <- putQuestStage Quests.theTrialOfIntuition
      staged Enemies.wolfsGuardian
      eid <- enemyOf Enemies.wolfsGuardian
      getModifiers eid >>= (`shouldSatisfy` elem CannotTakeDamage)

    it "Wolf Spirit gives Wolf enemies +1 attack while Wolf's Guardian is in play" . nightmare theThreeTrials $ \_ -> do
      staged Enemies.wolfSpirit
      eid <- enemyOf Enemies.wolfSpirit
      getEnemyAttack eid `shouldReturn` 5
      staged Enemies.wolfsGuardian
      getEnemyAttack eid `shouldReturn` 6

    it "Wolf Spirit engages the player engaged with Wolf's Guardian as the encounter phase begins" . nightmare theThreeTrials $ \pid -> do
      _ <- engaged pid Enemies.wolfsGuardian
      staged Enemies.wolfSpirit
      run $ CheckWindows [Window After (PhaseBegins EncounterPhase)]
      answerAll pid
      spirit <- enemyOf Enemies.wolfSpirit
      (.placement) <$> getEnemy spirit `shouldReturn` EngagedWith pid

    it "Haunted Pillars surges with no active location" . nightmare theThreeTrials $ \_ -> do
      reveal Locations.hauntedPillars [Locations.sacredForest]
      getsGame (length . (.entities.locations)) `shouldReturn` 2

    it "Sacred Forest heals a Guardian 2 for each time counter removed from it" . nightmare theThreeTrials $ \_ -> do
      staged Locations.sacredForest
      staged Enemies.boarsGuardian
      eid <- enemyOf Enemies.boarsGuardian
      run $ PlaceTokens GameSource (toTarget eid) TimeCounter 2
      run $ DealDamage GameSource (toTarget eid) 5
      run $ RemoveTokens GameSource (toTarget eid) TimeCounter 1
      getDamage (toTarget eid) `shouldReturn` 3

    it "Trial of Ancestors gives non-unique characters -1 willpower" . nightmare theThreeTrials $ \pid -> do
      _ <- putQuestStage Quests.theTrialOfStrength
      ally <- putAllyIntoPlay pid Allies.wanderingTook
      was <- getWillpower ally
      reveal Treacheries.trialOfAncestors []
      getWillpower ally `shouldReturn` was - 1

    it "Cursed Burden keeps the hero with a Key from questing" . nightmare theThreeTrials $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      key <- genEncounterCard Objectives.keyOfTheBoar
      run $ PutObjectiveIntoPlayAttached key eowyn
      reveal Treacheries.cursedBurden [Locations.sacredForest]
      answerAll pid
      getModifiers eowyn >>= (`shouldSatisfy` elem CannotCommitToQuest)

  describe "Trouble in Tharbad" do
    it "builds the nightmare deck" . nightmare troubleInTharbad $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.tharbadHideout, Locations.hiddenAlleyway, Enemies.bellachsHunter, Treacheries.tharbadPatrol]
        `shouldBe` [0, 0, 2, 3]

    it "raises each player's starting threat by 5" . nightmare troubleInTharbad $ \pid -> do
      was <- threat pid
      run $ DoStep 8 Setup
      threat pid `shouldReturn` was + 5

    it "places no progress on The Crossing at Tharbad while Bellach is in play" . nightmare troubleInTharbad $ \_ -> do
      staged Locations.theCrossingAtTharbad
      staged Enemies.bellachTroubleInTharbad
      crossing <- locationOf Locations.theCrossingAtTharbad
      getModifiers crossing >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "Orc Seeker's shadow lowers each elimination level by 5 when undefended" . nightmare troubleInTharbad $ \pid -> do
      eid <- engaged pid Enemies.bloodHound
      answerAll pid
      shadow pid eid Enemies.orcSeeker []
      getsGame (eliminationLowering . scenarioMeta . toAttrs . (.scenario)) `shouldReturn` 5

    it "Blood Hound raises the threat by 5 as it engages" . nightmare troubleInTharbad $ \pid -> do
      was <- threat pid
      _ <- engaged pid Enemies.bloodHound
      answerAll pid
      threat pid `shouldReturn` was + 5

    it "Waylaid by Agents gains doomed 3 with a threat of 20 or less" . nightmare troubleInTharbad $ \pid -> do
      run $ LowerThreat pid 50
      reveal Treacheries.waylaidByAgents []
      answerAll pid
      threat pid `shouldReturn` 3

    it "Tharbad Patrol engages the enemies each player discards as they travel to its location" . nightmare troubleInTharbad $ \pid -> do
      _ <- setEncounterDeck [Treacheries.tharbadPatrol, Locations.cityOfSpies, Enemies.orcSeeker, Locations.cityOfSpies]
      staged Locations.darkPathway
      run RevealEncounterCard >> resolveRevealed
      answerAll pid
      pathway <- locationOf Locations.darkPathway
      run $ TravelTo pathway
      answerAll pid
      seeker <- enemyOf Enemies.orcSeeker
      (.placement) <$> getEnemy seeker `shouldReturn` EngagedWith pid

    it "Dark Pathway has the first treachery revealed in the quest phase surge" . nightmare troubleInTharbad $ \_ -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.darkPathway
      putGame . (phaseL .~ QuestPhase) =<< getGame
      reveal Treacheries.waylaidByAgents [Locations.cityOfSpies, Locations.cityOfSpies]
      getsGame (length . (.entities.locations)) `shouldReturn` 2
