module LordOfTheRings.Dwarrowdelf.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getStagingThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario (roadToRivendell, theRedhornGate, theWatcherInTheWater)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (scenarioQuestDeck, scenarioSetAside), encounterDiscardL, modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

inPlayCount :: CardDef -> GameT Int
inPlayCount def = getsGame \g ->
  length . filter (== toCardCode def)
    $ map toCardCode (toList g.entities.enemies)
    <> map toCardCode (toList g.entities.locations)
    <> map toCardCode (toList g.entities.treacheries)

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

setEncounterDiscard :: [CardDef] -> GameT ()
setEncounterDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

setAsideCards :: GameT [EncounterCard]
setAsideCards = getsGame (scenarioSetAside . toAttrs . (.scenario))

spec :: Spec
spec = describe "Dwarrowdelf nightmare decks" do
  describe "The Redhorn Gate" do
    it "builds the nightmare deck and sets the Blinding Blizzards aside" . nightmare theRedhornGate $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 44
      map (`copies` deck) [Enemies.mountainGoblin, Enemies.blackUruks, Enemies.silvertineMarauder, Treacheries.blindingBlizzard]
        `shouldBe` [0, 3, 4, 0]
      copies Treacheries.blindingBlizzard <$> setAsideCards `shouldReturn` 2

    it "shuffles the Blinding Blizzards in as stage 2b is revealed" . nightmare theRedhornGate $ \pid -> do
      run $ DoStep 1 Setup
      stage <- putQuestStage Quests.snowdrifts
      run $ Flip stage
      resolveWhenRevealed pid
      copies Treacheries.blindingBlizzard <$> getEncounterDeck `shouldReturn` 2

    it "Wolfpelt Goblin deals the active location's remaining quest points as damage" . nightmare theRedhornGate $ \pid -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.frozenSilverlode
      lid <- locationOf Locations.frozenSilverlode
      run $ PlaceTokens GameSource (toTarget lid) Progress 1
      reveal Enemies.wolfpeltGoblin [Locations.rockyCrags]
      eid <- enemyOf Enemies.wolfpeltGoblin
      run $ EngageEnemy pid eid
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 2
      (.progress) <$> getLocation lid `shouldReturn` 0

    it "Silvertine Marauder's shadow strikes a defender of little willpower" . nightmare theRedhornGate $ \pid -> do
      reveal Enemies.snowWarg []
      eid <- enemyOf Enemies.snowWarg
      theodred <- toTarget <$> getHeroWithCode "01002"
      shadow pid eid Enemies.silvertineMarauder [theodred]
      getEnemyAttack eid `shouldReturn` 5

    it "Icy Cavern's threat is the Snow cards in the staging area" . nightmare theRedhornGate $ \_ -> do
      reveal Locations.icyCavern []
      reveal Enemies.snowWarg []
      getStagingThreat `shouldReturn` 5

    it "Blinding Blizzard takes the quest's progress and leaves at 5" . nightmare theRedhornGate $ \_ -> do
      quest <- putQuestStage Quests.upThePass
      reveal Treacheries.blindingBlizzard []
      inPlayCount Treacheries.blindingBlizzard `shouldReturn` 1
      run $ PlaceTokens GameSource quest Progress 3
      (.progress) <$> getActiveQuest `shouldReturn` 0
      run $ PlaceTokens GameSource quest Progress 2
      inPlayCount Treacheries.blindingBlizzard `shouldReturn` 0

    it "Caradhras the Cruel returns the Snow cards and reveals one" . nightmare theRedhornGate $ \_ -> do
      setEncounterDiscard [Enemies.snowWarg, Treacheries.fellVoices]
      reveal Treacheries.caradhrasTheCruel [Locations.rockyCrags]
      inPlayCount Enemies.snowWarg `shouldReturn` 1
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.rockyCrags]

  describe "Road to Rivendell" do
    it "builds the nightmare deck with Ambush in the Night for stage 2" . nightmare roadToRivendell $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 49
      map (`copies` deck) [Enemies.goblinSpearman, Enemies.goblinTaskmaster, Enemies.urukElfStalker]
        `shouldBe` [0, 3, 3]
      questDeck <- getsGame (scenarioQuestDeck . toAttrs . (.scenario))
      map toCardCode questDeck `shouldBe` map toCardCode [Quests.ambushInTheNight, Quests.approachingRivendell]

    it "Ambush in the Night brings out Goblin Gate and an enemy, then guards Arwen" . nightmare roadToRivendell $ \pid -> do
      reveal Objectives.arwenUndomielRoadToRivendell []
      arwen <- getsGame (map toId . toList . (.entities.objectives))
      for_ arwen \oid -> run $ PutObjectiveAllyIntoPlay oid pid
      _ <- setEncounterDeck [Locations.goblinGate, Locations.pathlessCountry, Enemies.orcRaiders]
      stage <- putQuestStage Quests.ambushInTheNight
      run $ Flip stage
      -- the enemy's and the stage's When Revealed effects
      resolveRevealed
      gate <- locationOf Locations.goblinGate
      inPlayCount Enemies.orcRaiders `shouldReturn` 1
      allies <- getsGame (toList . (.entities.allies))
      map (.placement) allies `shouldBe` [AttachedTo (toTarget gate)]
      run $ PlaceTokens GameSource stage Progress 3
      (.progress) <$> getActiveQuest `shouldReturn` 0

    it "Vicious Trackers makes its player's threat 20 higher for engagement checks" . nightmare roadToRivendell $ \pid -> do
      reveal Enemies.viciousTrackers [Enemies.raidingParty]
      tracker <- enemyOf Enemies.viciousTrackers
      run $ EngageEnemy pid tracker
      party <- enemyOf Enemies.raidingParty
      run $ RaiseThreat pid 20
      run $ EngagementCheck pid 0
      (.placement) <$> getEnemy party `shouldReturn` EngagedWith pid

    it "Tumultuous Assault returns the ambushers engaged for new engagement checks" . nightmare roadToRivendell $ \pid -> do
      reveal Enemies.goblinTrapper []
      trapper <- enemyOf Enemies.goblinTrapper
      run $ EngageEnemy pid trapper
      chooseTarget pid =<< (toTarget <$> getHeroWithCode "01007")
      reveal Treacheries.tumultuousAssault []
      (.placement) <$> getEnemy trapper `shouldReturn` StagingArea

    it "an ambusher engaging in the quest phase adds its threat to the staging area" . nightmare roadToRivendell $ \pid -> do
      reveal Enemies.goblinTrapper []
      trapper <- enemyOf Enemies.goblinTrapper
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ EngageEnemy pid trapper
      chooseTarget pid =<< (toTarget <$> getHeroWithCode "01007")
      getStagingThreat `shouldReturn` 3

    it "Treacherous Bluffs gets +2 threat as an ambusher engages" . nightmare roadToRivendell $ \pid -> do
      reveal Locations.treacherousBluffs []
      reveal Enemies.raidingParty []
      party <- enemyOf Enemies.raidingParty
      run $ EngageEnemy pid party
      getStagingThreat `shouldReturn` 3

  describe "The Watcher in the Water" do
    it "builds the nightmare deck with the nightmare Watcher" . nightmare theWatcherInTheWater $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 52
      map (`copies` deck) [Enemies.theWatcher, Enemies.theWatcherNightmare, Enemies.chokingTentacle]
        `shouldBe` [0, 1, 4]

    it "a Tentacle enemy destroyed deals 1 damage to The Watcher" . nightmare theWatcherInTheWater $ \_ -> do
      reveal Enemies.theWatcherNightmare []
      reveal Enemies.chokingTentacle []
      watcher <- enemyOf Enemies.theWatcherNightmare
      tentacle <- enemyOf Enemies.chokingTentacle
      run $ DealDamage GameSource (toTarget tentacle) 4
      getDamage (toTarget watcher) `shouldReturn` 1

    it "The Watcher cannot be engaged while Doors of Durin is in play" . nightmare theWatcherInTheWater $ \pid -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.doorsOfDurin
      reveal Enemies.theWatcherNightmare []
      watcher <- enemyOf Enemies.theWatcherNightmare
      getModifiers watcher >>= (`shouldSatisfy` elem (CannotEngagePlayer pid))

    it "Writhing Tentacle attacks as it engages and takes hold of the defender" . nightmare theWatcherInTheWater $ \pid -> do
      reveal Enemies.writhingTentacle [Treacheries.pulledUnder]
      tentacle <- enemyOf Enemies.writhingTentacle
      run $ EngageEnemy pid tentacle
      beravor <- toTarget <$> getHeroWithCode "01012"
      passActions pid
      chooseTarget pid beravor
      passActions pid
      resolveShadowsInOrder pid
      passActions pid
      (.placement) <$> getEnemy tentacle `shouldReturn` AttachedTo beravor
      getModifiers beravor >>= (`shouldSatisfy` elem CannotCommitToQuest)

    it "Pulled Under deals 3 damage to each character a Tentacle holds" . nightmare theWatcherInTheWater $ \_ -> do
      reveal Enemies.chokingTentacle []
      tentacle <- enemyOf Enemies.chokingTentacle
      beravor <- toTarget <$> getHeroWithCode "01012"
      run $ PlaceEnemy tentacle (AttachedTo beravor)
      reveal Treacheries.pulledUnder []
      getCharacterDamage beravor `shouldReturn` 3

    it "Rippling From Beneath shuffles a Tentacle back and heals The Watcher" . nightmare theWatcherInTheWater $ \_ -> do
      reveal Enemies.theWatcherNightmare []
      watcher <- enemyOf Enemies.theWatcherNightmare
      run $ PlaceTokens GameSource (toTarget watcher) Damage 2
      setEncounterDiscard [Enemies.chokingTentacle, Enemies.writhingTentacle]
      reveal Treacheries.ripplingFromBeneath [Locations.banksOfSirannon]
      discarded <- getEncounterDiscard
      copies Enemies.chokingTentacle discarded + copies Enemies.writhingTentacle discarded `shouldBe` 1
      getDamage (toTarget watcher) `shouldReturn` 1

    it "Banks of Sirannon gives the Tentacles +2 defense" . nightmare theWatcherInTheWater $ \_ -> do
      reveal Locations.banksOfSirannon []
      reveal Enemies.chokingTentacle []
      (getEnemyDefense =<< enemyOf Enemies.chokingTentacle) `shouldReturn` 3
