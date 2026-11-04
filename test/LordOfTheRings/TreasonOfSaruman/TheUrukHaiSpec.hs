module LordOfTheRings.TreasonOfSaruman.TheUrukHaiSpec (spec) where

import LordOfTheRings.Campaign
import LordOfTheRings.Card (CardDef (cdEncounterSet), HasCardCode, IsCard (toCard), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theUrukHai)
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getPursuit)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkWhen)
import TestImport

urukTest :: (PlayerId -> GameT ()) -> IO ()
urukTest = gameTestIn theUrukHai testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup, Théodred chosen as the Captive.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  chooseTarget pid =<< hero "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

captives :: GameT [CardCode]
captives = getsGame (map toCardCode . filter (isJust . heroGuardedBy . toAttrs) . toList . (.entities.heroes))

guardOf :: CardCode -> GameT (Maybe CardId)
guardOf code = heroGuardedBy . toAttrs <$> getHeroWithCode code

cardIdOf :: CardDef -> GameT CardId
cardIdOf def = do
  es <- getsGame (.entities)
  let match :: (IsCard a, HasCardCode a) => [a] -> Maybe CardId
      match = fmap ((.id) . toCard) . find ((== toCardCode def) . toCardCode)
  pure $ fromJustNote "no such card" $ match (toList es.objectives) <|> match (toList es.enemies) <|> match (toList es.locations)

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

-- | The enemy attacks the player, the shadow card dealt to it first.
shadowAttack :: PlayerId -> EnemyId -> CardDef -> GameT ()
shadowAttack pid eid shadow = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid

setPursuitTo :: Int -> GameT ()
setPursuitTo n = run $ ScenarioSpecific "urukHai.setPursuit" (toJSON n)

-- | The Lord of the Rings campaign, The Road Darkens won, the test deck's heroes recorded.
lotr :: CampaignLog
lotr =
  (newCampaignLog "the-lord-of-the-rings")
    { campaignLogHeroes = [["01012", "01007", "01002"]]
    , campaignLogCompleted =
        [ CompletedScenario sid 100
        | sid <- ["a-shadow-of-the-past", "a-knife-in-the-dark", "flight-to-the-ford", "the-ring-goes-south", "journey-in-the-dark", "breaking-of-the-fellowship"]
        ]
    }

-- | Stage 1 defeated, nothing found for stage 2's search, and Uglúk gone.
toStageTwo :: PlayerId -> GameT ()
toStageTwo pid = do
  _ <- setEncounterDeck []
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress 10
  resolveWhenRevealed pid
  chooseOptionMatching "found nothing" pid (const True)
  ugluk <- enemyOf Enemies.ugluk
  run $ Discard (EnemyTarget ugluk)

spec :: Spec
spec = describe "The Uruk-hai" do
  it "is a scenario the game can be dealt" . urukTest $ \_ ->
    isJust (lookupScenario "the-uruk-hai") `shouldBe` True

  describe "setup" do
    it "makes Eastemnet active, stages Uglúk and To Isengard!, takes a Captive and sets the pursuit value to 10" . urukTest $ \pid -> do
      setUp pid
      captives `shouldReturn` ["01002"]
      toIsengard <- cardIdOf Objectives.toIsengard
      guardOf "01002" `shouldReturn` Just toIsengard
      eastemnet <- locationOf Locations.eastemnet
      (.placement) <$> getsGame (fromJustNote "eastemnet" . lookup eastemnet . (.entities.locations)) `shouldReturn` ActiveLocation
      ugluk <- enemyOf Enemies.ugluk
      placementOfEnemy ugluk `shouldReturn` StagingArea
      getPursuit `shouldReturn` 10
      q <- toAttrs <$> getActiveQuest
      toCardCode q `shouldBe` toCardCode Quests.followTheOrcs
      getQuestPoints q `shouldReturn` 10
      getModifiers (EnemyTarget ugluk) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [CannotLeaveStagingArea, CannotTakeDamage, NotFoughtByAllies, CannotHaveAttachments])
      -- the Fellowship Aragorn is the first player's, and no Captive
      _ <- getHeroWithCode "143001"
      attrs <- scenarioAttrs
      attrs.scenarioSetAside `shouldBe` []
      map toCardCode attrs.questDeck `shouldBe` [toCardCode Quests.rescueTheCaptives]
      all ((`elem` map Just [TheUrukHai, OrcsOfTheWhiteHand, SnagaOrcs]) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True

    it "treats the Captive's text box as blank, and offers no Fellowship hero" . urukTest $ \pid -> do
      flipFirstQuest pid
      aragorn <- hero "143001"
      offers <- maybe False (any \case TargetLabel t _ -> t == aragorn; _ -> False) . fmap questionChoices <$> getQuestion pid
      offers `shouldBe` False
      theodred <- hero "01002"
      chooseTarget pid theodred
      getModifiers theodred >>= (`shouldSatisfy` elem Blank)

    it "builds the encounter deck in normal and easy mode" . urukTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.ugluksUrukHai, Enemies.isengardUruk, Enemies.urukHaiFighter, Enemies.snaga, Enemies.grishnakh, Locations.plainsOfRohan, Locations.eastemnet, Treacheries.strangeWeariness, Treacheries.fightingUrukHai, Treacheries.sarumansOrders]
        `shouldBe` [3, 3, 3, 3, 1, 4, 2, 2, 2, 2]

    it "builds the easy mode encounter deck" . gameTestIn (overAttrs (modeL .~ EasyMode) theUrukHai) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.ugluksUrukHai, Enemies.isengardUruk, Enemies.snaga, Locations.plainsOfRohan, Treacheries.strangeWeariness, Treacheries.fightingUrukHai, Treacheries.sarumansOrders]
        `shouldBe` [1, 2, 2, 3, 1, 1, 1]

  describe "To Isengard!" do
    it "increases the pursuit value at the end of the refresh phase, and loses the game at 30" . urukTest $ \pid -> do
      setUp pid
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      useForcedAbilities pid
      getPursuit `shouldReturn` 11
      outcome `shouldReturn` Nothing
      setPursuitTo 29
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      useForcedAbilities pid
      outcome `shouldReturn` Just Lost

    it "loses the game when no Captive is left in play" . urukTest $ \pid -> do
      setUp pid
      theodred <- hero "01002"
      run $ DealDamage GameSource theodred 10
      outcome `shouldReturn` Just Lost

  describe "Follow the Orcs" do
    it "increases the pursuit value by 1 for each enemy in the staging area at the end of the combat phase" . urukTest $ \pid -> do
      setUp pid
      _ <- stage Enemies.snaga
      run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
      useForcedAbilities pid
      getPursuit `shouldReturn` 12

    it "is defeated once its progress reaches the pursuit value, and Rescue the Captives reveals an Uruk-hai enemy" . urukTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Enemies.isengardUruk, Locations.theWold]
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 9
      activeQuestCode `shouldReturn` toCardCode Quests.followTheOrcs
      run $ PlaceTokens GameSource q Progress 1
      activeQuestCode `shouldReturn` toCardCode Quests.rescueTheCaptives
      resolveWhenRevealed pid
      chooseOptionMatching "take the Uruk" pid \case TargetLabel {} -> True; _ -> False
      resolveRevealed
      chooseLabel "$label.isengardUruk.reveal" pid
      resolveRevealed
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.isengardUruk))

  describe "Rescue the Captives" do
    it "reduces the pursuit value as an enemy is defeated, cannot be defeated while Uglúk is in play, and wins once he is gone" . urukTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck []
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 10
      resolveWhenRevealed pid
      chooseOptionMatching "found nothing" pid (const True)
      activeQuestCode `shouldReturn` toCardCode Quests.rescueTheCaptives
      snaga <- stage Enemies.snaga
      run $ DealDamage GameSource (EnemyTarget snaga) 3
      chooseAbility pid
      getPursuit `shouldReturn` 9
      stage2 <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource stage2 Progress 9
      outcome `shouldReturn` Nothing
      ugluk <- enemyOf Enemies.ugluk
      run $ Discard (EnemyTarget ugluk)
      outcome `shouldReturn` Just Won

  describe "Grishnákh" do
    it "takes a Captive as he enters play, damages it at the end of the quest phase, and leaves it under To Isengard! as he is defeated" . urukTest $ \pid -> do
      setUp pid
      grishnakh <- stage Enemies.grishnakh
      useForcedAbilities pid
      grishnakhCard <- cardIdOf Enemies.grishnakh
      guardOf "01002" `shouldReturn` Just grishnakhCard
      theodred <- hero "01002"
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      useForcedAbilities pid
      getCharacterDamage theodred `shouldReturn` 1
      toIsengard <- cardIdOf Objectives.toIsengard
      run $ DealDamage GameSource (EnemyTarget grishnakh) 5
      useForcedAbilities pid
      enemyCodes >>= (`shouldSatisfy` notElem (toCardCode Enemies.grishnakh))
      guardOf "01002" `shouldReturn` Just toIsengard

  describe "the encounter cards" do
    it "has Uglúk's Uruk-hai increase the pursuit value by 3 or reveal another card" . urukTest $ \pid -> do
      setUp pid
      reveal [Enemies.ugluksUrukHai]
      chooseLabel "$label.ugluksUrukHai.pursuit" pid
      getPursuit `shouldReturn` 13

    it "has Isengard Uruk assign 3 damage among the revealing player's characters" . urukTest $ \pid -> do
      setUp pid
      reveal [Enemies.isengardUruk]
      chooseLabel "$label.isengardUruk.damage" pid
      beravor <- hero "01012"
      replicateM_ 3 $ chooseTarget pid beravor
      getCharacterDamage beravor `shouldReturn` 3

    it "has Snaga gain the threat of the card it discards from hand" . urukTest $ \pid -> do
      setUp pid
      g <- getGame
      putGame $ g & playersL . ix pid . handL .~ []
      _ <- putCardIntoHand pid (toCardDef ("01016" :: CardCode))
      reveal [Enemies.snaga]
      handSize pid `shouldReturn` 0
      snaga <- enemyOf Enemies.snaga
      -- Snowbourn Scout costs 1
      getEnemyThreat snaga `shouldReturn` (1 + 1)

    it "makes The Wold and Eastemnet raise the pursuit value as the players travel there, and answer their exploring while active" . urukTest $ \pid -> do
      setUp pid
      eastemnet <- locationOf Locations.eastemnet
      pursuit0 <- getPursuit
      beravor <- hero "01012"
      resources <- getHeroResources . (\case HeroTarget h -> h; _ -> error "hero") $ beravor
      run $ PlaceTokens GameSource (LocationTarget eastemnet) Progress 3
      chooseAbility pid
      getHeroResources (case beravor of HeroTarget h -> h; _ -> error "hero") `shouldReturn` (resources + 1)
      wold <- stageLocation Locations.theWold
      run $ TravelTo wold
      getPursuit `shouldReturn` (pursuit0 + 1)
      cards <- handSize pid
      run $ PlaceTokens GameSource (LocationTarget wold) Progress 2
      chooseAbility pid
      handSize pid `shouldReturn` (cards + 1)

    it "gives Plains of Rohan X threat for the locations in the staging area" . urukTest $ \pid -> do
      setUp pid
      plains <- stageLocation Locations.plainsOfRohan
      _ <- stageLocation Locations.theWold
      getLocationThreat plains `shouldReturn` 2

    it "has Banks of Entwash increase the pursuit value as progress is placed on it" . urukTest $ \pid -> do
      setUp pid
      banks <- stageLocation Locations.banksOfEntwash
      run $ PlaceTokens GameSource (LocationTarget banks) Progress 1
      useForcedAbilities pid
      getPursuit `shouldReturn` 11

    it "has Rest by Night end the quest phase and increase the pursuit value by 3" . urukTest $ \pid -> do
      setUp pid
      -- nothing is committed, so ending the quest phase is the only option
      reveal [Treacheries.restByNight]
      getPursuit `shouldReturn` 13

    it "has Rest by Night damage each committed character instead" . urukTest $ \pid -> do
      setUp pid
      beravor <- hero "01012"
      run $ CommitToQuest beravor
      reveal [Treacheries.restByNight]
      chooseLabel "$label.restByNight.damage" pid
      getCharacterDamage beravor `shouldReturn` 1

    it "has Unseen Barrier hold the current quest to 3 progress this round" . urukTest $ \pid -> do
      setUp pid
      reveal [Treacheries.unseenBarrier]
      q <- toAttrs <$> getActiveQuest
      getModifiers (QuestTarget q.id) >>= (`shouldSatisfy` elem (ProgressPerRoundLimit 3))

    it "has Fighting Uruk-hai put an Uruk-hai enemy into play engaged with the revealer, adding engaged threat" . urukTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Treacheries.fightingUrukHai, Enemies.urukHaiFighter]
      run RevealEncounterCard
      resolveRevealed
      chooseOptionMatching "take the Uruk" pid \case TargetLabel {} -> True; _ -> False
      resolveRevealed
      fighter <- enemyOf Enemies.urukHaiFighter
      placementOfEnemy fighter `shouldReturn` EngagedWith pid
      getModifiers GameTarget >>= (`shouldSatisfy` any \case EngagedEnemiesAddThreat _ -> True; _ -> False)

    it "has Saruman's Orders return an engaged enemy to the staging area" . urukTest $ \pid -> do
      setUp pid
      snaga <- engaged pid Enemies.snaga
      reveal [Treacheries.sarumansOrders]
      chooseTarget pid (EnemyTarget snaga)
      placementOfEnemy snaga `shouldReturn` StagingArea

    it "has Banks of Entwash's shadow increase the pursuit value or leave the attack undefended" . urukTest $ \pid -> do
      setUp pid
      snaga <- engaged pid Enemies.snaga
      beravor <- hero "01012"
      shadowAttack pid snaga Locations.banksOfEntwash
      chooseTarget pid beravor
      passActions pid
      chooseLabel "$label.banksOfEntwash.pursuit" pid
      passActions pid
      getPursuit `shouldReturn` 11
      getCharacterDamage beravor `shouldReturn` 1

    it "has Plains of Rohan's shadow give the attacker +1 attack" . urukTest $ \pid -> do
      setUp pid
      snaga <- engaged pid Enemies.snaga
      beravor <- hero "01012"
      shadowAttack pid snaga Locations.plainsOfRohan
      chooseTarget pid beravor
      passActions pid
      getCharacterDamage beravor `shouldReturn` 2

    it "has Strange Weariness give each committed character -1 willpower this round" . urukTest $ \pid -> do
      setUp pid
      beravor <- hero "01012"
      run $ CommitToQuest beravor
      reveal [Treacheries.strangeWeariness]
      getModifiers beravor >>= (`shouldSatisfy` elem (WillpowerModifier (-1)))

  describe "campaign mode" do
    it "leaves out the burdens of A Shadow of the Past, Flight to the Ford and The Ring Goes South, keeping the others" . campaignTestIn theUrukHai lotr {campaignLogPool = [PoolCard (toCardCode Treacheries.theRingDrawsThem) Nothing Nothing, PoolCard (toCardCode Treacheries.illFate) Nothing Nothing]} testDeck $ \pid -> do
      setUp pid
      attrs <- scenarioAttrs
      copies Treacheries.theRingDrawsThem attrs.encounterDeck `shouldBe` 0
      copies Treacheries.illFate attrs.encounterDeck `shouldBe` 1

    it "removes Gildor Inglorion and Mr. Underhill from the Campaign Pool as it is set up" . campaignTestIn theUrukHai lotr {campaignLogPool = [PoolCard "141077" Nothing Nothing, PoolCard "141017" Nothing (Just "141001")]} testDeck $ \pid -> do
      setUp pid
      (.pool) <$> getCampaignLog `shouldReturn` []

    it "earns a Skill boon for each player, on a hero its Setup allows, with the pursuit value 25 or lower" . campaignTestIn theUrukHai lotr testDeck $ \pid -> do
      setUp pid
      toStageTwo pid
      setPursuitTo 12
      stage2 <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource stage2 Progress 12
      chooseLabel "$label.theUrukHai.skillBoons" pid
      chooseLabel "$label.theUrukHai.intimidation" pid
      outcome `shouldReturn` Just Won
      (.pool) <$> getCampaignLog `shouldReturn` [PoolCard "143015" Nothing (Just "01002")]

    it "brings a fallen hero back with Beyond All Hope, the player giving up a hero for him at no penalty" . campaignTestIn theUrukHai lotr {campaignLogFallenHeroes = ["01003"]} testDeck $ \pid -> do
      setUp pid
      toStageTwo pid
      setPursuitTo 12
      stage2 <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource stage2 Progress 12
      chooseLabel "$label.theUrukHai.beyondAllHope" pid
      chooseLabelStarting "$label.theUrukHai.fallenHero" pid
      chooseTarget pid =<< hero "01007"
      outcome `shouldReturn` Just Won
      l <- getCampaignLog
      l.fallenHeroes `shouldBe` []
      l.heroes `shouldBe` [["01012", "01002", "01003"]]
      l.pool `shouldBe` [PoolCard "143019" Nothing (Just "01003")]
      -- the next scenario's heroes, the fallen one among them, cost nothing
      fmap (.threatPenalty) (seatHeroes [["01012", "01002", "01003"]] l) `shouldBe` Right l.threatPenalty

    -- The card names no exception, so a hero Helm's Deep bars (Gríma) is still offered.
    it "offers any fallen hero to Beyond All Hope, even one the next scenario bars" . campaignTestIn theUrukHai lotr {campaignLogFallenHeroes = ["07002"]} testDeck $ \pid -> do
      setUp pid
      toStageTwo pid
      setPursuitTo 12
      stage2 <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource stage2 Progress 12
      chooseLabel "$label.theUrukHai.beyondAllHope" pid
      chooseLabelStarting "$label.theUrukHai.fallenHero" pid
      chooseTarget pid =<< hero "01007"
      outcome `shouldReturn` Just Won
      (.pool) <$> getCampaignLog `shouldReturn` [PoolCard "143019" Nothing (Just "07002")]

    it "earns nothing with the pursuit value above 25" . campaignTestIn theUrukHai lotr testDeck $ \pid -> do
      setUp pid
      toStageTwo pid
      setPursuitTo 26
      stage2 <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource stage2 Progress 26
      outcome `shouldReturn` Just Won
      (.pool) <$> getCampaignLog `shouldReturn` []
