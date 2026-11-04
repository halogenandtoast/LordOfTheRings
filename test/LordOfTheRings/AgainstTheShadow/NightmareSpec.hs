module LordOfTheRings.AgainstTheShadow.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy, getObjective)
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo, CannotCollectResources))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase, QuestPhase))
import LordOfTheRings.Scenario (assaultOnOsgiliath, encounterAtAmonDin, theBloodOfGondor, theDruadanForest, theMorgulVale, theStewardsFear)
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (Meta (..), getMeta, underneathCount)
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (deadVillagerCount, getVillagers, rescuedVillagerCount)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (towerProgress)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (scenarioSetAside, scenarioVictoryDisplay), metaL, modeL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (AdvancedToStage, PhaseBegins, PhaseEnds, RoundEnds), mkAfter)
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

-- | Rescued Villagers and Dead Villagers in the staging area, with villagers rescued.
villagers :: Int -> GameT ()
villagers rescued = do
  for_ [Objectives.rescuedVillagers, Objectives.deadVillagers] \d -> run . AddToStaging =<< genEncounterCard d
  oid <- getsGame (fmap toId . find ((== toCardCode Objectives.rescuedVillagers) . toCardCode) . toList . (.entities.objectives))
  when (rescued > 0) $ for_ oid \o -> run $ PlaceTokens GameSource (ObjectiveTarget o) Villager rescued

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

fearMeta :: GameT Meta
fearMeta = getsGame (getMeta . toAttrs . (.scenario))

-- | The underworld deck, made of these cards.
setUnderworld :: [CardDef] -> GameT [EncounterCard]
setUnderworld defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (metaL .~ toJSON (Meta cards mempty []))
  pure cards

objectiveOf :: CardDef -> GameT ObjectiveId
objectiveOf def = getsGame (fromJustNote "no such objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

staged' :: CardDef -> GameT ()
staged' def = run . AddToStaging =<< genEncounterCard def

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

  describe "The Drúadan Forest" do
    it "builds the nightmare deck, the nightmare Drû-buri-Drû in place of the standard one" . nightmare theDruadanForest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.druBuriDru, Locations.secludedGlade, Enemies.druBuriDruNightmare, Enemies.druadanWarrior]
        `shouldBe` [0, 0, 1, 3]

    it "gives each hero 5 resources, stages Garden of Poisons and sets the nightmare Drû-buri-Drû aside" . nightmare theDruadanForest $ \pid -> do
      flipFirstQuest pid
      resolveRevealed
      heroes <- getsGame (toList . (.entities.heroes))
      map (.resources) heroes `shouldBe` [5, 5, 5]
      getsGame (map toCardCode . toList . (.entities.locations)) >>= (`shouldSatisfy` elem (toCardCode Locations.gardenOfPoisons))
      getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.druBuriDruNightmare]

    it "heroes collect no resources" . nightmare theDruadanForest $ \_ -> do
      hid <- toId <$> getHeroWithCode "01012"
      run $ CheckWindows []
      getModifiers hid >>= (`shouldSatisfy` elem CannotCollectResources)

    it "tops the heroes up to 3 resources once stage 2 is revealed" . nightmare theDruadanForest $ \_ -> do
      _ <- putQuestStage Quests.anUntimelyEnd
      run $ CheckWindows [mkAfter AdvancedToStage]
      hid <- toId <$> getHeroWithCode "01012"
      getHeroResources hid `shouldReturn` 3

    it "Drû-buri-Drû takes no more than 1 damage each round" . nightmare theDruadanForest $ \_ -> do
      dru <- staged Enemies.druBuriDruNightmare
      run $ DealDamage GameSource (toTarget dru) 3
      run $ DealDamage GameSource (toTarget dru) 1
      (.damage) <$> getEnemy dru `shouldReturn` 1

    it "Drúadan Stalker costs 2 resources to engage optionally" . nightmare theDruadanForest $ \pid -> do
      stalker <- staged Enemies.druadanStalker
      run $ OptionalEngagement pid
      getQuestion pid `shouldReturn` Nothing
      hid <- toId <$> getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget hid) Resource 2
      run $ OptionalEngagement pid
      chooseTarget pid stalker
      getHeroResources hid `shouldReturn` 0
      (.placement) <$> getEnemy stalker `shouldReturn` EngagedWith pid

    it "Drums of the Wild Men prowls for engaged enemies and returns the Woses" . nightmare theDruadanForest $ \pid -> do
      warrior <- staged Enemies.druadanWarrior
      run $ EngageEnemy pid warrior
      hid <- toId <$> getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget hid) Resource 2
      reveal Treacheries.drumsOfTheWildMen []
      replicateM_ 2 $ chooseTarget pid (HeroTarget hid)
      getHeroResources hid `shouldReturn` 0
      (.placement) <$> getEnemy warrior `shouldReturn` StagingArea

    it "Poison-darts arms the enemy with the highest engagement cost" . nightmare theDruadanForest $ \_ -> do
      _ <- staged Enemies.druadanWarrior
      stalker <- staged Enemies.druadanStalker
      reveal Treacheries.poisonDarts []
      getEnemyAttack stalker `shouldReturn` 5

    it "Stonewain Valley takes the first player's spent resources and gains their threat" . nightmare theDruadanForest $ \_ -> do
      reveal Locations.stonewainValley []
      hid <- toId <$> getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget hid) Resource 2
      run $ RemoveTokens GameSource (toTarget hid) Resource 2
      getStagingThreat `shouldReturn` 3

    it "Eilenach cannot be traveled to while a hero has no resources" . nightmare theDruadanForest $ \_ -> do
      reveal Locations.eilenach []
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)

    it "Devil's Mirk in the staging area stops healing" . nightmare theDruadanForest $ \_ -> do
      reveal Locations.devilsMirk []
      beravor <- toTarget <$> getHeroWithCode "01012"
      run $ DealDamage GameSource beravor 2
      run $ HealDamage GameSource beravor 2
      getCharacterDamage beravor `shouldReturn` 2

  describe "Encounter at Amon Dîn" do
    it "builds the nightmare deck" . nightmare encounterAtAmonDin $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.orcRavager, Locations.burningFarmhouse, Enemies.pillagingOrc, Locations.besiegedHomestead]
        `shouldBe` [0, 2, 3, 3]

    it "places 1 villager token on Rescued Villagers at setup" . nightmare encounterAtAmonDin $ \pid -> do
      run Setup
      chooseLabel "$label.keepHand" pid
      _ <-
        setEncounterDeck
          [Enemies.ghulat, Objectives.lordAlcaron, Locations.burningFarmhouse, Objectives.rescuedVillagers, Objectives.deadVillagers, Locations.secludedFarmhouse]
      chooseOptionMatching "flip the quest" pid (const True)
      rescuedVillagerCount `shouldReturn` 1

    it "discards a rescued villager or reveals a card as the quest phase begins" . nightmare encounterAtAmonDin $ \pid -> do
      villagers 1
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      chooseLabel "$label.amonDin.discardVillager" pid
      rescuedVillagerCount `shouldReturn` 0
      deadVillagerCount `shouldReturn` 1

    it "keeps damage off Ghulat until Protect the Villagers has 15 progress" . nightmare encounterAtAmonDin $ \_ -> do
      quest <- putQuestStage Quests.protectTheVillagers
      ghulat <- staged Enemies.ghulat
      run $ DealDamage GameSource (toTarget ghulat) 1
      (.damage) <$> getEnemy ghulat `shouldReturn` 0
      run $ PlaceTokens GameSource quest Progress 15
      run $ DealDamage GameSource (toTarget ghulat) 1
      (.damage) <$> getEnemy ghulat `shouldReturn` 1

    it "Pillaging Orc reveals another card when no villager is rescued" . nightmare encounterAtAmonDin $ \_ -> do
      villagers 0
      reveal Enemies.pillagingOrc [Locations.besiegedHomestead]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "Orc Raiders takes a rescued villager instead of attacking" . nightmare encounterAtAmonDin $ \pid -> do
      villagers 1
      reveal Enemies.orcRaidersNightmare []
      chooseLabel "$label.amonDin.discardVillager" pid
      rescuedVillagerCount `shouldReturn` 0

    it "Trapped Villagers kills 3 villagers and adds them to the staging area's threat" . nightmare encounterAtAmonDin $ \_ -> do
      villagers 0
      reveal Treacheries.trappedVillagers []
      deadVillagerCount `shouldReturn` 3
      getStagingThreat `shouldReturn` 3

    it "Run for your Lives! deals damage for each dead villager" . nightmare encounterAtAmonDin $ \pid -> do
      villagers 1
      reveal Treacheries.runForYourLives []
      deadVillagerCount `shouldReturn` 1
      beravor <- toTarget <$> getHeroWithCode "01002"
      chooseTarget pid beravor
      getCharacterDamage beravor `shouldReturn` 1

    it "Besieged Homestead discards a villager from a staged location given progress" . nightmare encounterAtAmonDin $ \_ -> do
      villagers 0
      reveal Locations.besiegedHomestead []
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ PlaceTokens GameSource (toTarget lid) Progress 1
      getVillagers (toTarget lid) `shouldReturn` 1

    it "Overrun Village kills a villager as the players travel there" . nightmare encounterAtAmonDin $ \_ -> do
      villagers 0
      reveal Locations.overrunVillage []
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ TravelTo lid
      deadVillagerCount `shouldReturn` 1

    it "Those Now Sundered discards a rescued villager at the end of each round" . nightmare encounterAtAmonDin $ \_ -> do
      villagers 2
      reveal Treacheries.thoseNowSundered []
      endRound
      rescuedVillagerCount `shouldReturn` 1

    it "Save Us! with no villagers rescued searches out an enemy" . nightmare encounterAtAmonDin $ \_ -> do
      villagers 0
      reveal Treacheries.saveUs [Locations.besiegedHomestead, Enemies.orcRaidersNightmare]
      void $ enemyOf Enemies.orcRaidersNightmare

  describe "Assault on Osgiliath" do
    it "builds the nightmare deck" . nightmare assaultOnOsgiliath $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.urukSoldier, Enemies.southronMercenaries, Locations.ruinedSquare, Enemies.invadingOrc, Locations.palaceRemains]
        `shouldBe` [0, 0, 2, 3, 1]

    it "adds Palace Remains to the staging area at setup" . nightmare assaultOnOsgiliath $ \_ -> do
      run $ DoStep 1 Setup
      run $ DoStep 8 Setup
      lid <- locationOf Locations.palaceRemains
      (.placement) <$> getLocation lid `shouldReturn` StagingArea

    it "Palace Remains takes a resource as a player takes control of a location, its threat with them" . nightmare assaultOnOsgiliath $ \pid -> do
      reveal Locations.palaceRemains []
      quarter <- stagedLocation Locations.easternQuarter
      run $ PlaceLocation quarter (InPlayArea pid)
      palace <- locationOf Locations.palaceRemains
      getLocationThreat palace `shouldReturn` 1
      getModifiers palace >>= (`shouldSatisfy` elem CannotBeTraveledTo)

    it "Crumbled Battlements engages an enemy discarded from the deck as a player takes control of it" . nightmare assaultOnOsgiliath $ \pid -> do
      battlements <- stagedLocation Locations.crumbledBattlements
      _ <- setEncounterDeck [Treacheries.relentlessAttack, Enemies.invadingOrc]
      run $ PlaceLocation battlements (InPlayArea pid)
      orc <- enemyOf Enemies.invadingOrc
      (.placement) <$> getEnemy orc `shouldReturn` EngagedWith pid

    it "Haradrim Warrior's threat is the first player's locations" . nightmare assaultOnOsgiliath $ \pid -> do
      quarter <- stagedLocation Locations.westernQuarter
      run $ PlaceLocation quarter (InPlayArea pid)
      warrior <- staged Enemies.haradrimWarrior
      getEnemyThreat warrior `shouldReturn` 1

    it "Overrun Defenses adds the controlled locations to the staging area's threat and is doomed under 4" . nightmare assaultOnOsgiliath $ \pid -> do
      quarter <- stagedLocation Locations.westernQuarter
      run $ PlaceLocation quarter (InPlayArea pid)
      was <- (.threat) <$> getPlayer pid
      reveal Treacheries.overrunDefenses []
      getStagingThreat `shouldReturn` 1
      (.threat) <$> getPlayer pid `shouldReturn` was + 2

    it "Relentless Attack lowers engagement costs by the controlled locations" . nightmare assaultOnOsgiliath $ \pid -> do
      _ <- putQuestStage Quests.retakeTheCity
      quarter <- stagedLocation Locations.westernQuarter
      run $ PlaceLocation quarter (InPlayArea pid)
      reveal Treacheries.relentlessAttack []
      orc <- staged Enemies.invadingOrc
      getEngagementCost orc `shouldReturn` 19

    it "sends progress beyond the active location to the chosen location" . nightmare assaultOnOsgiliath $ \pid -> do
      for_ [Locations.crumbledBattlements, Locations.easternQuarter] \d -> void $ stagedLocation d
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      battlements <- locationOf Locations.crumbledBattlements
      chooseTarget pid battlements
      run $ PlaceProgressOnQuest 1
      (.progress) <$> getLocation battlements `shouldReturn` 1

  describe "The Steward's Fear" do
    it "shuffles False Accusations into the underworld deck and keeps the removed Plots" . nightmare theStewardsFear $ \pid -> do
      flipFirstQuest pid
      resolveWhenRevealed pid
      m <- fearMeta
      let underworld = m.underworldDeck <> concat (toList m.underneath)
      length underworld `shouldBe` 11
      copies Treacheries.falseAccusations underworld `shouldBe` 2
      copies Enemies.pickpocket underworld `shouldBe` 0
      length m.removedPlots `shouldBe` 4

    it "Murderous Turncoat goes under the active location at the end of the combat phase" . nightmare theStewardsFear $ \_ -> do
      _ <- setUnderworld []
      reveal Locations.rathCelerdain []
      lid <- locationOf Locations.rathCelerdain
      run $ PlaceLocation lid ActiveLocation
      _ <- staged Enemies.murderousTurncoat
      run $ CheckWindows [Window When (PhaseEnds CombatPhase)]
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      underneathCount lid `shouldReturn` 1

    it "Faithless Conspirator takes no damage while the active location has a card under it" . nightmare theStewardsFear $ \_ -> do
      _ <- setUnderworld [Enemies.lossarnachBandit]
      reveal Locations.archedTunnel []
      lid <- locationOf Locations.archedTunnel
      run $ PlaceLocation lid ActiveLocation
      conspirator <- staged Enemies.faithlessConspirator
      run $ DealDamage GameSource (toTarget conspirator) 1
      (.damage) <$> getEnemy conspirator `shouldReturn` 0

    it "Arched Tunnel reveals what is under it and takes the next underworld card" . nightmare theStewardsFear $ \_ -> do
      _ <- setUnderworld [Enemies.lossarnachBandit, Enemies.zealousTraitor]
      reveal Locations.archedTunnel []
      lid <- locationOf Locations.archedTunnel
      run $ TravelTo lid
      void $ enemyOf Enemies.lossarnachBandit
      underneathCount lid `shouldReturn` 1
      (.underworldDeck) <$> fearMeta `shouldReturn` []

    it "Traitors' Den reveals a removed Plot at its fourth resource" . nightmare theStewardsFear $ \_ -> do
      plot <- genEncounterCard Objectives.riseToPower
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (metaL .~ toJSON (Meta [] mempty [plot]))
      reveal Locations.traitorsDen []
      lid <- locationOf Locations.traitorsDen
      run $ PlaceTokens GameSource (toTarget lid) Resource 3
      endRound
      void $ objectiveOf Objectives.riseToPower
      (.removedPlots) <$> fearMeta `shouldReturn` []

    it "Rath Celerdain reveals the top underworld card as the encounter phase begins" . nightmare theStewardsFear $ \_ -> do
      _ <- setUnderworld [Enemies.lossarnachBandit]
      reveal Locations.rathCelerdain []
      run $ CheckWindows [mkAfter $ PhaseBegins EncounterPhase]
      void $ enemyOf Enemies.lossarnachBandit

    it "False Accusations raises a player's threat by 5 if he chooses" . nightmare theStewardsFear $ \pid -> do
      was <- (.threat) <$> getPlayer pid
      reveal Treacheries.falseAccusations []
      chooseLabel "$label.falseAccusations.threat" pid
      (.threat) <$> getPlayer pid `shouldReturn` was + 5

    it "Rise to Power gains 2 resources each round and adds them to the quest phase's threat" . nightmare theStewardsFear $ \_ -> do
      staged' Objectives.riseToPower
      endRound
      run $ BeginPhase QuestPhase
      getStagingThreat `shouldReturn` 2

    it "Assassination marks the hero with the highest threat cost and arms each enemy" . nightmare theStewardsFear $ \_ -> do
      bandit <- staged Enemies.lossarnachBandit
      unarmed <- getEnemyAttack bandit
      staged' Objectives.assassination
      gloin <- toTarget <$> getHeroWithCode "01012"
      oid <- objectiveOf Objectives.assassination
      (.placement) <$> getObjective oid `shouldReturn` AttachedTo gloin
      getEnemyAttack bandit `shouldReturn` unarmed + 1

    it "The Cabal's Champion goes to the Villain" . nightmare theStewardsFear $ \_ -> do
      villain <- staged Enemies.daughterOfBeruthiel
      staged' Objectives.theCabalsChampion
      oid <- objectiveOf Objectives.theCabalsChampion
      (.placement) <$> getObjective oid `shouldReturn` AttachedTo (toTarget villain)

    it "shuffles a destroyed enemy into the underworld deck" . nightmare theStewardsFear $ \_ -> do
      _ <- setUnderworld []
      bandit <- staged Enemies.lossarnachBandit
      run $ DealDamage GameSource (toTarget bandit) 5
      map toCardCode . (.underworldDeck) <$> fearMeta `shouldReturn` [toCardCode Enemies.lossarnachBandit]

    it "shuffles a destroyed enemy with victory points into the underworld deck from the victory display" . nightmare theStewardsFear $ \_ -> do
      _ <- setUnderworld []
      troll <- staged Enemies.hillTroll
      run $ DealDamage GameSource (toTarget troll) 9
      map toCardCode . (.underworldDeck) <$> fearMeta `shouldReturn` [toCardCode Enemies.hillTroll]
      getsGame (scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` []
