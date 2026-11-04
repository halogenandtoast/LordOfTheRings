module LordOfTheRings.Core.NightmareSpec (spec) where

import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs, modeL, scenarioMode)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

theEnemy :: GameT EnemyId
theEnemy = getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))

theLocation :: GameT LocationId
theLocation = getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

io :: IO () -> IO ()
io = id

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "Nightmare mode" do
  it "loads a game saved before modes, easy mode kept" $ io do
    let saved = toJSON (toAttrs passageThroughMirkwood)
        legacy = case saved of
          Object o -> Object $ KeyMap.insert "easyMode" (Bool True) $ KeyMap.delete "mode" o
          v -> v
    (scenarioMode <$> fromJSON @ScenarioAttrs legacy) `shouldBe` Success EasyMode

  describe "Passage Through Mirkwood" do
    it "builds the nightmare deck: removals out, the nightmare set in" . nightmare passageThroughMirkwood $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 39
      map (`copies` deck) [Enemies.dolGuldurOrcs, Enemies.forestSpider, Locations.oldForestRoad, Enemies.ungoliantsSpawn, Enemies.ungoliantsBrood]
        `shouldBe` [0, 1, 1, 0, 3]
      copies Nightmare.passageThroughMirkwoodNightmare deck `shouldBe` 0

    it "has each player reveal a card once the quest is set up" . nightmare passageThroughMirkwood $ \pid -> do
      run Setup
      chooseLabel "$label.keepHand" pid
      _ <- setEncounterDeck [Locations.forestGate, Enemies.forestSpider, Locations.oldForestRoad]
      chooseOptionMatching "flip the quest" pid (const True)
      getsGame (length . (.entities.locations)) `shouldReturn` 2
      getsGame (length . (.entities.enemies)) `shouldReturn` 1

    it "leaves a standard game's encounter deck alone" . gameTestIn passageThroughMirkwood testDeck $ \_ -> do
      run $ DoStep 1 Setup
      length <$> getEncounterDeck `shouldReturn` 36

    it "Ungoliant's Spawn's shadow raises the defender's threat by 10 when undefended" . nightmare passageThroughMirkwood $ \pid -> do
      reveal Enemies.wargs []
      eid <- theEnemy
      was <- threat pid
      shadow pid eid Enemies.ungoliantsSpawnNightmare []
      threat pid `shouldReturn` was + 10

    it "Spiders of Mirkwood gets +1 attack for each exhausted character of its player" . nightmare passageThroughMirkwood $ \pid -> do
      reveal Enemies.spidersOfMirkwood []
      eid <- theEnemy
      run $ EngageEnemy pid eid
      run . Exhaust . toTarget =<< getHeroWithCode "01007"
      getEnemyAttack eid `shouldReturn` 3

    it "Backtrack! returns the topmost enemy or location from the discard pile" . nightmare passageThroughMirkwood $ \_ -> do
      [camp] <- traverse genEncounterCard [Locations.abandonedCampNightmare]
      run $ AddToEncounterDiscard [camp]
      reveal Treacheries.backtrack [Treacheries.theSpidersWeb]
      locations <- getsGame (map toCardCode . toList . (.entities.locations))
      locations `shouldBe` [toCardCode Locations.abandonedCampNightmare]

    it "Ungoliant's Brood keeps its player's cards from readying this round, those entering play too" . nightmare passageThroughMirkwood $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.ungoliantsBrood
      brood <- theEnemy
      run $ EngageEnemy pid brood
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ Exhaust eowyn
      run $ Ready eowyn
      getsGame (fmap (heroExhausted . toAttrs) . find ((== "01007") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` Just True
      ally <- putAllyIntoPlay pid Allies.silverlodeArcher
      run $ Exhaust ally
      run $ Ready ally
      getsGame (fmap (allyExhausted . toAttrs) . headMay . toList . (.entities.allies)) `shouldReturn` Just True

    it "The Spider's Web damages its hero each time it readies" . nightmare passageThroughMirkwood $ \pid -> do
      reveal Treacheries.theSpidersWeb []
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      run $ Ready eowyn
      getCharacterDamage eowyn `shouldReturn` 1

  describe "Journey Along the Anduin" do
    it "builds the nightmare deck" . nightmare journeyAlongTheAnduin $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 47
      map (`copies` deck) [Enemies.mistyMountainGoblins, Enemies.hillTroll, Enemies.anduinTrollSpawn]
        `shouldBe` [0, 2, 3]

    it "resets the empty encounter deck outside the quest phase" . nightmare journeyAlongTheAnduin $ \_ -> do
      _ <- setEncounterDeck [Locations.aFloodedFord]
      [discarded] <- traverse genEncounterCard [Locations.gladdenMarshlands]
      run $ AddToEncounterDiscard [discarded]
      -- the game begins in the resource phase
      run RevealEncounterCard
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.gladdenMarshlands]

    it "keeps attachments off Trolls" . nightmare journeyAlongTheAnduin $ \_ -> do
      reveal Enemies.hillTroll []
      eid <- theEnemy
      getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)

    it "Anduin Troll Spawn surges with no Hill Troll in play" . nightmare journeyAlongTheAnduin $ \_ -> do
      reveal Enemies.anduinTrollSpawn [Locations.aFloodedFord]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "Brown Water Rats cannot be damaged" . nightmare journeyAlongTheAnduin $ \_ -> do
      reveal Enemies.brownWaterRats []
      eid <- theEnemy
      run $ DealDamage GameSource (toTarget eid) 3
      getsGame (length . (.entities.enemies)) `shouldReturn` 1

    it "Brown Water Rats leave once the last enemy without the Rat trait leaves play at stage 3" . nightmare journeyAlongTheAnduin $ \_ -> do
      _ <- putQuestStage Quests.ambushOnTheShore
      run . AddToStaging =<< genEncounterCard Enemies.brownWaterRats
      run . AddToStaging =<< genEncounterCard Enemies.hillTroll
      troll <- getsGame (fromJustNote "no troll" . fmap toId . find ((== toCardCode Enemies.hillTroll) . toCardCode) . toList . (.entities.enemies))
      getsGame (length . (.entities.enemies)) `shouldReturn` 2
      run $ Discard (toTarget troll)
      getsGame (length . (.entities.enemies)) `shouldReturn` 0

    it "Marshland Outlaws keeps its player's card effects from damaging other enemies" . nightmare journeyAlongTheAnduin $ \pid -> do
      reveal Enemies.marshlandOutlaws []
      outlaws <- theEnemy
      run $ EngageEnemy pid outlaws
      run . AddToStaging =<< genEncounterCard Enemies.hillTroll
      troll <- getsGame (fromJustNote "no troll" . fmap toId . find ((== toCardCode Enemies.hillTroll) . toCardCode) . toList . (.entities.enemies))
      eowyn <- getHeroWithCode "01007"
      run $ DealDamage (HeroSource eowyn.id) (toTarget troll) 2
      getDamage (toTarget troll) `shouldReturn` 0
      run $ DealDamage (HeroSource eowyn.id) (toTarget outlaws) 2
      getDamage (toTarget outlaws) `shouldReturn` 2

    it "Pursuit on the Shore returns the strongest enemy from the discard pile" . nightmare journeyAlongTheAnduin $ \_ -> do
      cards <- traverse genEncounterCard [Enemies.wargs, Enemies.hillTroll]
      run $ AddToEncounterDiscard cards
      reveal Treacheries.pursuitOnTheShore []
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.hillTroll]

    it "Smoking Blood raises the threat by the damage removed from engaged enemies" . nightmare journeyAlongTheAnduin $ \pid -> do
      reveal Enemies.hillTroll []
      eid <- theEnemy
      run $ EngageEnemy pid eid
      run $ DealDamage GameSource (toTarget eid) 3
      was <- threat pid
      reveal Treacheries.smokingBlood []
      threat pid `shouldReturn` was + 3

    it "Smoking Blood's shadow, undefended, moves the enemy's damage to the hero the attack damaged" . nightmare journeyAlongTheAnduin $ \pid -> do
      reveal Enemies.hillTroll []
      eid <- theEnemy
      run $ EngageEnemy pid eid
      run $ DealDamage GameSource (toTarget eid) 3
      hero <- getsGame (toTarget . fromJustNote "no hero" . headMay . toList . (.entities.heroes))
      run $ CreateEffect $ Effect GameSource hero [HitPointModifier 20] UntilEndOfPhase
      attack <- getEnemyAttack eid
      _ <- setEncounterDeck [Treacheries.smokingBlood]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      chooseTarget pid hero
      passActions pid
      getCharacterDamage hero `shouldReturn` attack + 3
      (.damage) <$> getEnemy eid `shouldReturn` 0

    it "Troll Attack's shadow deals a Troll's attack to each other player, undefended, with its damage" $ twoPlayerTest testDeck \one two -> do
      run . PutEnemyIntoPlayEngaged one =<< genEncounterCard Enemies.hillTroll
      troll <- theEnemy
      attack <- getEnemyAttack troll
      theirs <- getsGame (map toTarget . filter ((== two) . (.owner)) . toList . (.entities.heroes))
      for_ theirs \h -> run $ CreateEffect $ Effect GameSource h [HitPointModifier 20] UntilEndOfPhase
      _ <- setEncounterDeck [Treacheries.trollAttack]
      run $ DealShadowCard troll
      run $ EnemyAttack one troll
      let answer = do
            for_ [one, two] passActions
            q1 <- getQuestion one
            q2 <- getQuestion two
            case (q1, q2) of
              (Just (QuestionLabel l _ _ _), _) | "$label.declareDefender" `isPrefixOf` l -> chooseLabel "$label.takeAttackUndefended" one >> answer
              (_, Just (QuestionLabel "$label.defendWithSentinel" _ _ _)) -> chooseLabel "$label.doNotDefend" two >> answer
              (Just (QuestionLabel "$label.undefendedDamage" _ _ _), _) -> chooseOptionMatching "damage" one (const True) >> answer
              (_, Just (QuestionLabel "$label.undefendedDamage" _ _ _)) -> chooseOptionMatching "damage" two (const True) >> answer
              (Just (QuestionLabel "$label.takeAction" _ _ _), _) -> passActions one >> answer
              (_, Just (QuestionLabel "$label.takeAction" _ _ _)) -> passActions two >> answer
              _ -> pure ()
      answer
      sum <$> traverse getCharacterDamage theirs `shouldReturn` attack

  describe "Escape from Dol Guldur" do
    it "builds the nightmare deck" . nightmare escapeFromDolGuldur $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 43
      map (`copies` deck) [Enemies.dolGuldurOrcs, Locations.towerGate, Enemies.tortureMaster]
        `shouldBe` [1, 0, 3]

    it "Torture Chamber loses the game at 4 resources" . nightmare escapeFromDolGuldur $ \_ -> do
      reveal Locations.tortureChamber []
      lid <- theLocation
      run $ PlaceTokens GameSource (toTarget lid) Resource 4
      getsGame (.outcome) `shouldReturn` Just Lost

    it "Dungeon Labyrinth gets +1 threat for each resource on it" . nightmare escapeFromDolGuldur $ \_ -> do
      reveal Locations.dungeonLabyrinth []
      lid <- theLocation
      run $ PlaceTokens GameSource (toTarget lid) Resource 2
      getStagingThreat `shouldReturn` 3

    it "Crazed Captive's shadow raises the threat by 7 at the end of the phase" . nightmare escapeFromDolGuldur $ \pid -> do
      reveal Enemies.wargs []
      eid <- theEnemy
      was <- threat pid
      shadow pid eid Enemies.crazedCaptive []
      run $ EndPhase CombatPhase
      -- and 1 more as the refresh phase follows
      threat pid `shouldReturn` was + 8

    it "Sentinel of Shadow adds twice the players to the staging area's threat" . nightmare escapeFromDolGuldur $ \_ -> do
      reveal Treacheries.sentinelOfShadow []
      getStagingThreat `shouldReturn` 2

    it "Torture Master's shadow takes a damaged hero prisoner" . nightmare escapeFromDolGuldur $ \pid -> do
      reveal Enemies.wargs []
      eid <- theEnemy
      shadow pid eid Enemies.tortureMaster []
      eowyn <- getHeroWithCode "01007"
      run $ DealDamage (EnemyAttackSource eid) (toTarget eowyn) 1
      heroPrisoner . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
