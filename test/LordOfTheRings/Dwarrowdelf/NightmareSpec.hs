module LordOfTheRings.Dwarrowdelf.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyAttack, getEnemyDefense, getStagingThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyEngagedWith, EnemyWithId), PlayerMatcher (PlayerWithId), getEnemy, getThreat, selectAny)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Scenario (foundationsOfStone, roadToRivendell, shadowAndFlame, theLongDark, theRedhornGate, theWatcherInTheWater)
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

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

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

    it "Snow-Giant in the staging area attacks the first player, not attackable as if engaged" . nightmare theRedhornGate $ \pid -> do
      reveal Enemies.snowGiant []
      giant <- enemyOf Enemies.snowGiant
      selectAny (EnemyWithId giant <> EnemyEngagedWith (PlayerWithId pid)) `shouldReturn` False
      run $ EnemyAttacks pid
      passActions pid
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.declareDefender" `isPrefixOf` l; _ -> False)

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

    it "Vicious Trackers' 20 threat counts in Raiding Party's check against a discarded ambusher" . nightmare roadToRivendell $ \pid -> do
      reveal Enemies.viciousTrackers [Enemies.raidingParty]
      run . EngageEnemy pid =<< enemyOf Enemies.viciousTrackers
      -- 15 threat, short of the Goblin Trapper's 30 but for the Trackers' 20
      run . RaiseThreat pid . (15 -) =<< getThreat pid
      _ <- setEncounterDeck [Enemies.goblinTrapper]
      run . EngageEnemy pid =<< enemyOf Enemies.raidingParty
      resolveRevealed
      (.placement) <$> (getEnemy =<< enemyOf Enemies.goblinTrapper) `shouldReturn` EngagedWith pid

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

    it "The Watcher in the staging area takes no damage from player cards, only that" . nightmare theWatcherInTheWater $ \_ -> do
      reveal Enemies.theWatcherNightmare []
      watcher <- enemyOf Enemies.theWatcherNightmare
      eowyn <- getHeroWithCode "01007"
      run $ DealDamage (HeroSource eowyn.id) (toTarget watcher) 2
      getDamage (toTarget watcher) `shouldReturn` 0
      getModifiers watcher >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)
      run $ DealDamage GameSource (toTarget watcher) 2
      getDamage (toTarget watcher) `shouldReturn` 2

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

  describe "The Long Dark" do
    it "builds the nightmare deck" . nightmare theLongDark $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 56
      map (`copies` deck) [Enemies.goblinSneak, Enemies.strayGoblin, Enemies.goblinStalker]
        `shouldBe` [0, 1, 4]

    it "its setup card's Lost: has the first player discard an ally" . nightmare theLongDark $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.snowbournScout
      run $ TriggerLostEffects GameSource
      getsGame (length . (.entities.allies)) `shouldReturn` 0

    it "Dreadful Deep has each player trigger a Lost: effect, as Lost and Helpless's" . nightmare theLongDark $ \pid -> do
      reveal Treacheries.lostAndHelpless [Locations.easternMines]
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      helpless <- getsGame (map toTarget . toList . (.entities.treacheries))
      reveal Treacheries.dreadfulDeep []
      for_ helpless (chooseTarget pid)
      getsGame (length . (.entities.heroes)) `shouldReturn` 2

    it "Goblin Stalker gets +2 attack when dealt a shadow card with the pass icon, as it turns faceup" . nightmare theLongDark $ \pid -> do
      reveal Enemies.goblinStalker []
      eid <- enemyOf Enemies.goblinStalker
      run $ EngageEnemy pid eid
      run . AttachShadowCard eid =<< genEncounterCard Locations.dwarvenForge
      getEnemyAttack eid `shouldReturn` 2
      run $ ResolveShadow eid pid []
      getEnemyAttack eid `shouldReturn` 4

    it "Sentinel of the Deep stops its player drawing from any card effect" . nightmare theLongDark $ \pid -> do
      reveal Enemies.sentinelOfTheDeep []
      eid <- enemyOf Enemies.sentinelOfTheDeep
      run $ EngageEnemy pid eid
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotDrawCardsFromCardEffects)

    it "Trapdoor Spider discards the character it damages" . nightmare theLongDark $ \_ -> do
      reveal Enemies.trapdoorSpider []
      eid <- enemyOf Enemies.trapdoorSpider
      beravor <- toTarget <$> getHeroWithCode "01012"
      run $ DealDamage (EnemyAttackSource eid) beravor 1
      getsGame (length . (.entities.heroes)) `shouldReturn` 2

  describe "Foundations of Stone" do
    it "builds the nightmare deck, the nightmare set waiting with Foundations of Stone" . nightmare foundationsOfStone $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      aside <- setAsideCards
      (length deck, length aside) `shouldBe` (28, 35)
      map (`copies` aside) [Enemies.moriaBats, Enemies.unspeakableThing, Locations.drownedTreasury]
        `shouldBe` [0, 3, 0]

    it "Mithril Seeker gets +1 attack for each resource on it" . nightmare foundationsOfStone $ \_ -> do
      reveal Enemies.mithrilSeeker []
      eid <- enemyOf Enemies.mithrilSeeker
      run $ PlaceTokens GameSource (toTarget eid) Resource 2
      getEnemyAttack eid `shouldReturn` 4

    it "Unspeakable Thing takes the top card of its player's deck as it engages" . nightmare foundationsOfStone $ \pid -> do
      reveal Enemies.unspeakableThing []
      eid <- enemyOf Enemies.unspeakableThing
      getEnemyAttack eid `shouldReturn` 5
      deckSize <- length . (.deck) <$> getPlayer pid
      run $ EngageEnemy pid eid
      length . (.deck) <$> getPlayer pid `shouldReturn` deckSize - 1

    it "Primeval Thing takes cards from the first player's deck until they cost 6" . nightmare foundationsOfStone $ \pid -> do
      reveal Enemies.primevalThing []
      eid <- enemyOf Enemies.primevalThing
      getEnemyAttack eid >>= (`shouldSatisfy` (>= 6))
      deckSize <- length . (.deck) <$> getPlayer pid
      deckSize `shouldSatisfy` (< 50)

    it "Chill of the Roots surges when no hand is discarded" . nightmare foundationsOfStone $ \pid -> do
      reveal Treacheries.chillOfTheRoots [Locations.nestOfHorrors]
      chooseLabel "$label.chillOfTheRoots.raiseThreat" pid
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "Washed Away! has each player discard allies down to a total cost of 6" . nightmare foundationsOfStone $ \pid -> do
      gandalf <- putAllyIntoPlay pid Allies.gandalf
      passResponses pid
      _ <- putAllyIntoPlay pid Allies.snowbournScout
      passResponses pid
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      passResponses pid
      stage <- putQuestStage Quests.washedAway
      run $ DoStep 2 (WhenRevealed stage)
      chooseTarget pid gandalf
      getsGame (length . (.entities.allies)) `shouldReturn` 2

  describe "Shadow and Flame" do
    it "builds the nightmare deck, the Attack cards in a Balrog deck of their own" . nightmare shadowAndFlame $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 49
      map (`copies` deck) [Enemies.rangingGoblin, Treacheries.terribleStrength, Enemies.servantOfFlame]
        `shouldBe` [0, 0, 3]

    it "deals Durin's Bane its shadow cards from the Balrog deck, discarded apart" . nightmare shadowAndFlame $ \_ -> do
      run $ DoStep 1 Setup
      reveal Enemies.durinsBane []
      bane <- enemyOf Enemies.durinsBane
      run $ DealShadowCard bane
      shadows <- (.shadowCards) <$> getEnemy bane
      map toCardCode shadows `shouldSatisfy` all (`elem` map toCardCode [Treacheries.terribleStrength, Treacheries.theDarkFire, Treacheries.blazingGrip, Treacheries.morgothsBidding, Treacheries.flameOfUdun])
      run $ DiscardShadowCards bane
      getEncounterDiscard `shouldReturn` []

    it "Captain of Mordor lowers each enemy's engagement cost by 20" . nightmare shadowAndFlame $ \_ -> do
      reveal Enemies.captainOfMordor []
      (getEngagementCost =<< enemyOf Enemies.captainOfMordor) `shouldReturn` 22

    it "Servant of Flame is stronger while Durin's Bane is unharmed" . nightmare shadowAndFlame $ \_ -> do
      reveal Enemies.durinsBane []
      reveal Enemies.servantOfFlame []
      servant <- enemyOf Enemies.servantOfFlame
      getEnemyAttack servant `shouldReturn` 5
      bane <- enemyOf Enemies.durinsBane
      run $ PlaceTokens GameSource (toTarget bane) Damage 1
      getEnemyAttack servant `shouldReturn` 3

    it "Crumbling Stairs makes events cost 1 more" . nightmare shadowAndFlame $ \_ -> do
      reveal Locations.crumblingStairs []
      getModifiers GameTarget >>= (`shouldSatisfy` any (\case IncreaseCost _ 1 -> True; _ -> False))
