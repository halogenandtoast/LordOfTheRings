module LordOfTheRings.LostRealm.NightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithTitle), cardMatches)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (deadmensDike, intrudersInChetwood, theWeatherHills)
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (Meta (..), captiveVillagerHeld, getMeta)
import LordOfTheRings.Scenario.Types (GameMode (..), metaL, modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | The Weather Hills' Orc deck, top first.
setOrcDeck :: [CardDef] -> GameT ()
setOrcDeck defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (metaL .~ toJSON (Meta cards))) =<< getGame

placementOfAlly :: AllyId -> GameT (Maybe Placement)
placementOfAlly aid = getsGame (fmap (.placement) . find ((== aid) . toId) . toList . (.entities.allies))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

discarded :: [CardDef] -> GameT ()
discarded defs = run . AddToEncounterDiscard =<< traverse genEncounterCard defs

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

questOf :: CardDef -> GameT Target
questOf def = getsGame (fromJustNote "no such quest" . fmap toTarget . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

isInPlay :: CardDef -> GameT Bool
isInPlay def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ any ((== code) . toCardCode) (toList g.entities.quests)
    || any ((== code) . toCardCode) (toList g.entities.enemies)
    || any ((== code) . toCardCode) (toList g.entities.locations)

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "The Lost Realm Nightmare" do
  describe "Intruders in Chetwood" do
    it "builds the nightmare deck: removals out, the nightmare set in" . nightmare intrudersInChetwood $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.angmarMarauder, Locations.chetwoodForest, Treacheries.surprisingSpeed, Enemies.orcAbductor, Quests.protectTheBreeLanders]
        `shouldBe` [0, 1, 0, 3, 1]

    it "Orc Abductor reveals Rescue Iârion from the discard pile at the end of the round" . nightmare intrudersInChetwood $ \_ -> do
      staged Enemies.orcAbductor
      discarded [Quests.rescueIarion]
      run $ CheckWindows [mkAfter RoundEnds]
      isInPlay Quests.rescueIarion `shouldReturn` True

    it "Soldier of Angmar reveals Orc Ambush after its attack destroys an ally" . nightmare intrudersInChetwood $ \pid -> do
      staged Enemies.soldierOfAngmar
      eid <- enemyOf Enemies.soldierOfAngmar
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      discarded [Quests.orcAmbush]
      run $ CheckWindows [mkAfter $ CharacterDestroyedByAttack eid ally pid]
      isInPlay Quests.orcAmbush `shouldReturn` True

    it "Northern Chetwood reveals Lost in the Wilderness as progress is placed on it" . nightmare intrudersInChetwood $ \_ -> do
      staged Locations.northernChetwood
      lid <- locationOf Locations.northernChetwood
      discarded [Quests.lostInTheWilderness]
      run $ PlaceTokens GameSource (toTarget lid) Progress 1
      isInPlay Quests.lostInTheWilderness `shouldReturn` True

    it "Outskirts of Archet reveals Protect the Bree-landers as the players travel there" . nightmare intrudersInChetwood $ \_ -> do
      _ <- putQuestStage Quests.stopTheWarParty
      staged Locations.outskirtsOfArchet
      lid <- locationOf Locations.outskirtsOfArchet
      _ <- setEncounterDeck [Quests.protectTheBreeLanders]
      run $ TravelTo lid
      isInPlay Quests.protectTheBreeLanders `shouldReturn` True

    it "Shadow the War Party shields each Orc War Party" . nightmare intrudersInChetwood $ \pid -> do
      staged Enemies.orcWarParty
      eid <- enemyOf Enemies.orcWarParty
      run $ EngageEnemy pid eid
      reveal [Quests.shadowTheWarParty]
      placement <- getsGame (fmap (.placement) . find ((== eid) . toId) . toList . (.entities.enemies))
      placement `shouldBe` Just StagingArea
      mods <- getModifiers eid
      mods `shouldSatisfy` \ms -> all (`elem` ms) [CannotTakeDamage, ThreatModifier 2, CannotEngagePlayer pid]

    it "Protect the Bree-landers gains a resource and raises threat when no progress is placed" . nightmare intrudersInChetwood $ \pid -> do
      staged Quests.protectTheBreeLanders
      was <- threat pid
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      threat pid `shouldReturn` was + 1
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      threat pid `shouldReturn` was + 3

    it "Kill Them! stages an Orc War Party and removes the main quest's progress" . nightmare intrudersInChetwood $ \pid -> do
      q <- putQuestStage Quests.stopTheWarParty
      run $ PlaceTokens GameSource q Progress 5
      discarded [Enemies.orcWarParty]
      reveal [Treacheries.killThem]
      chooseLabel "$label.killThem.removeProgress" pid
      isInPlay Enemies.orcWarParty `shouldReturn` True
      (.progress) <$> getActiveQuest `shouldReturn` 0

    it "A Great Host returns engaged enemies and deals 4 damage when chosen" . nightmare intrudersInChetwood $ \pid -> do
      staged Enemies.angmarOrc
      eid <- enemyOf Enemies.angmarOrc
      run $ EngageEnemy pid eid
      reveal [Treacheries.aGreatHost]
      chooseLabel "$label.aGreatHost.damage" pid
      -- 4 damage destroys any of the test deck's heroes
      chooseTarget pid =<< toTarget <$> getHeroWithCode "01012"
      getsGame (length . (.entities.heroes)) `shouldReturn` 2
      getStagingThreat `shouldReturn` 2

    it "Northern Chetwood's shadow discards from hand for the damage dealt" . nightmare intrudersInChetwood $ \pid -> do
      staged Enemies.angmarOrc
      eid <- enemyOf Enemies.angmarOrc
      traverse_ (putCardIntoHand pid) [Allies.guardOfTheCitadel, Allies.guardOfTheCitadel, Allies.guardOfTheCitadel]
      hand <- length . (.hand) <$> getPlayer pid
      shadow pid eid Locations.northernChetwood []
      eowyn <- getHeroWithCode "01007"
      run $ DealDamage (EnemyAttackSource eid) (toTarget eowyn) 2
      length . (.hand) <$> getPlayer pid `shouldReturn` hand - 2

  describe "The Weather Hills" do
    it "builds the nightmare deck, the Orc deck taking the Captive Villagers and Rearguard Ambushers" . nightmare theWeatherHills $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      orcs <- getsGame ((.orcDeck) . getMeta . toAttrs . (.scenario))
      map (`copies` deck) [Enemies.corneredOrc, Locations.ruinsOfArnor, Enemies.angmarRaider, Objectives.captiveVillager]
        `shouldBe` [2, 0, 4, 0]
      map (`copies` orcs) [Objectives.captiveVillager, Enemies.rearguardAmbusher, Enemies.angmarOrc, Locations.concealedOrcCamp]
        `shouldBe` [3, 3, 1, 2]

    it "guards a Captive Villager with the Orc deck's top card, and frees it to the first player" . nightmare theWeatherHills $ \pid -> do
      setOrcDeck [Enemies.angmarCaptain]
      run . RevealCard =<< genEncounterCard Objectives.captiveVillager
      resolveRevealed
      captain <- enemyOf Enemies.angmarCaptain
      captiveVillagerHeld `shouldReturn` True
      run $ Discard (EnemyTarget captain)
      run $ CheckWindows []
      captiveVillagerHeld `shouldReturn` False
      villagers <- getsGame (filter ((== toCardCode Objectives.captiveVillager) . toCardCode) . toList . (.entities.allies))
      map (.placement) villagers `shouldBe` [InPlayArea pid]

    it "loses the game as a freed Captive Villager leaves play" . nightmare theWeatherHills $ \pid -> do
      setOrcDeck []
      run . RevealCard =<< genEncounterCard Objectives.captiveVillager
      villager <- getsGame (fromJustNote "no villager" . fmap toId . headMay . toList . (.entities.allies))
      placementOfAlly villager `shouldReturn` Just (InPlayArea pid)
      run $ Discard (AllyTarget villager)
      getsGame (.outcome) `shouldReturn` Just Lost

    it "Rearguard Ambusher exhausts each damaged character as it engages" . nightmare theWeatherHills $ \pid -> do
      staged Enemies.rearguardAmbusher
      eid <- enemyOf Enemies.rearguardAmbusher
      eowyn <- getHeroWithCode "01007"
      run $ DealDamage GameSource (toTarget eowyn) 1
      run $ EngageEnemy pid eid
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True

    it "Angmar Raider attacks as a character of its player is healed" . nightmare theWeatherHills $ \pid -> do
      staged Enemies.angmarRaider
      eid <- enemyOf Enemies.angmarRaider
      run $ EngageEnemy pid eid
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ DealDamage GameSource eowyn 2
      run $ HealDamage GameSource eowyn 1
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "Weather-torn Valley takes no progress in the staging area" . nightmare theWeatherHills $ \_ -> do
      staged Locations.weatherTornValley
      lid <- locationOf Locations.weatherTornValley
      getModifiers lid >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "Ruined Watch-tower, active, gives each enemy +3 defense" . nightmare theWeatherHills $ \_ -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.ruinedWatchTower
      staged Enemies.angmarOrc
      eid <- enemyOf Enemies.angmarOrc
      getModifiers eid >>= (`shouldSatisfy` elem (DefenseModifier 3))

    it "Rain-washed Tracks adds 8 to the staging area's threat without a Mission resource" . nightmare theWeatherHills $ \_ -> do
      reveal [Treacheries.rainWashedTracks]
      getStagingThreat `shouldReturn` 8

    it "Track the Orcs reveals the Orc deck's top card and raises engagement costs" . nightmare theWeatherHills $ \_ -> do
      setOrcDeck [Enemies.angmarCaptain]
      reveal [Quests.trackTheOrcs]
      eid <- enemyOf Enemies.angmarCaptain
      getModifiers eid >>= (`shouldSatisfy` elem (EngagementCostModifier 10))

  describe "Deadmen's Dike" do
    it "builds the nightmare deck, a Cursed Dead in the discard pile for each player" . nightmare deadmensDike $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.fornostSquare, Locations.brokenBattlements, Enemies.undeadHorde, Locations.fornostStreet]
        `shouldBe` [1, 0, 2, 4]
      discard <- getEncounterDiscard
      length discard `shouldBe` 1
      discard `shouldSatisfy` all (cardMatches $ CardWithTitle "Cursed Dead")

    it "Undead Horde discards from the deck for its excess damage" . nightmare deadmensDike $ \_ -> do
      staged Enemies.undeadHorde
      eid <- enemyOf Enemies.undeadHorde
      getModifiers eid >>= (`shouldSatisfy` \ms -> all (`elem` ms) [CannotHaveAttachments, ExcessDamageDiscardsFromDeck])

    it "Cursed Dead brings each copy, standard or not, from the discard pile" . nightmare deadmensDike $ \_ -> do
      discarded [Enemies.cursedDead]
      reveal [Enemies.cursedDeadNightmare]
      getsGame (length . (.entities.enemies)) `shouldReturn` 2

    it "Fornost Street raises other Fornost locations and returns an enemy as the players travel" . nightmare deadmensDike $ \_ -> do
      staged Locations.fornostStreet
      staged Locations.decrepitCity
      city <- locationOf Locations.decrepitCity
      getLocationThreat city `shouldReturn` 6
      discarded [Enemies.balefulShade]
      street <- locationOf Locations.fornostStreet
      run $ TravelTo street
      getsGame (length . (.entities.enemies)) `shouldReturn` 1

    it "Paralyzing Fear keeps its hero exhausted unless its player discards from his deck" . nightmare deadmensDike $ \pid -> do
      reveal [Treacheries.paralyzingFear]
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
      n <- length . (.deck) <$> getPlayer pid
      run $ Ready eowyn
      chooseLabel "$label.paralyzingFear.discard" pid
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False
      length . (.deck) <$> getPlayer pid `shouldReturn` n - 1

    it "Curse of Angmar discards the hand's copies of the discard pile's cards" . nightmare deadmensDike $ \pid -> do
      card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
      run $ PutInDiscard card
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      hand <- length . (.hand) <$> getPlayer pid
      reveal [Treacheries.curseOfAngmar]
      length . (.hand) <$> getPlayer pid `shouldReturn` hand - 1

    it "Gather Your Courage holds 8 cards of each deck until it is defeated" . nightmare deadmensDike $ \pid -> do
      n <- length . (.deck) <$> getPlayer pid
      reveal [Quests.gatherYourCourage, Locations.decrepitCity]
      length . (.deck) <$> getPlayer pid `shouldReturn` n - 8
      q <- questOf Quests.gatherYourCourage
      run $ PlaceTokens GameSource q Progress 8
      length . (.deck) <$> getPlayer pid `shouldReturn` n

    it "Palace Ruins' action discards from the deck to lower its threat" . nightmare deadmensDike $ \pid -> do
      staged Locations.palaceRuins
      lid <- locationOf Locations.palaceRuins
      n <- length . (.deck) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid lid
      chooseOptionMatching "choose 2" pid \case
        Label l _ -> "2" `isSuffixOf` l
        _ -> False
      getLocationThreat lid `shouldReturn` 7
      length . (.deck) <$> getPlayer pid `shouldReturn` n - 2
