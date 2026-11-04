module LordOfTheRings.OnTheDoorstep.FliesAndSpidersSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (newCampaignLog)
import LordOfTheRings.Card (cdCardCode, cdEncounterSet, toCardDef)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.EncounterSet (EncounterSet (FliesAndSpiders, Wilderland))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (heroesL)
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getConditionAttachments)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (fliesAndSpiders, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL)
import LordOfTheRings.Sphere (Sphere (Baggins))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

fsTest :: (PlayerId -> GameT ()) -> IO ()
fsTest = gameTestIn fliesAndSpiders testDeck

-- | A table of two in Flies and Spiders, both with the test deck.
fsTwoPlayerTest :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
fsTwoPlayerTest body = do
  seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", testDeck)])
  env <- newGameEnv (newTableIn fliesAndSpiders seats) (const $ pure ()) 1
  runGameT env do
    order <- getsGame (.playerOrder)
    case order of
      [one, two] -> body one two
      _ -> error "two players"

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup, Hidden Path the card stage 1A reveals, Bilbo's Magic Ring left out.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseLabel "$label.keepHand" pid
  _ <- setEncounterDeck [Locations.hiddenPath]
  chooseOptionMatching "flip the quest" pid (const True)
  chooseLabel "$label.intoMirkwood.leaveOut" pid

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

bilbo :: GameT Target
bilbo = hero "132001"

beravor :: GameT Target
beravor = hero "01012"

poison :: Target -> GameT Int
poison = \case
  HeroTarget hid -> length . heroPoison . toAttrs <$> getsGame (fromJustNote "no hero" . Map.lookup hid . (.entities.heroes))
  AllyTarget aid -> length . allyPoison . toAttrs <$> getsGame (fromJustNote "no ally" . Map.lookup aid . (.entities.allies))
  _ -> pure 0

unconscious :: Target -> GameT Bool
unconscious = \case
  HeroTarget hid -> heroUnconscious . toAttrs <$> getsGame (fromJustNote "no hero" . Map.lookup hid . (.entities.heroes))
  AllyTarget aid -> allyUnconscious . toAttrs <$> getsGame (fromJustNote "no ally" . Map.lookup aid . (.entities.allies))
  _ -> pure False

exhausted :: Target -> GameT Bool
exhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "no hero" . Map.lookup hid . (.entities.heroes))
  AllyTarget aid -> allyExhausted . toAttrs <$> getsGame (fromJustNote "no ally" . Map.lookup aid . (.entities.allies))
  _ -> pure False

discardSize :: PlayerId -> GameT Int
discardSize pid = length . (.discard) <$> getPlayer pid

deckSize :: PlayerId -> GameT Int
deckSize pid = length . (.deck) <$> getPlayer pid

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

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

questCodes :: GameT [CardCode]
questCodes = getsGame (map toCardCode . toList . (.entities.quests))

questDeck :: GameT [CardCode]
questDeck = getsGame (map toCardCode . scenarioQuestDeck . toAttrs . (.scenario))

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

-- | The enemy attacks the player, the shadow card dealt to it first.
shadowAttack :: PlayerId -> EnemyId -> CardDef -> GameT ()
shadowAttack pid eid shadow = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid

-- | The players in each area, in turn order, or none while the players are together.
areas :: GameT [[PlayerId]]
areas = getsGame \g -> maybe [] (map areaPlayers . areasInTurnOrder g) (gameSeparation g)

-- | A stage put into play on its B side, as the players reach it.
reachStage :: CardDef -> GameT Target
reachStage def = do
  run . PutQuestIntoPlay =<< genEncounterCard def
  q <- getsGame (toTarget . fromJustNote "no such stage" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))
  run $ Flip q
  resolveRevealed
  pure q

spec :: Spec
spec = describe "Flies and Spiders" do
  it "is a scenario the game can be dealt" . fsTest $ \_ ->
    isJust (lookupScenario "flies-and-spiders") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "gives the first player Bilbo Baggins, sets The Spider's Glade aside and reveals 1 card per player" . fsTest $ \pid -> do
      setUp pid
      b <- getHeroWithCode "132001"
      b.owner `shouldBe` pid
      attrs <- scenarioAttrs
      map toCardCode attrs.scenarioSetAside `shouldBe` [toCardCode Locations.theSpidersGlade]
      map toCardCode attrs.questDeck `shouldBe` map cdCardCode [Quests.lostInTheDarkFliesAndSpiders, Quests.capturedBySpiders, Quests.battleWithTheSpiders]
      questCodes `shouldReturn` [toCardCode Quests.intoMirkwood]
      locationCodes `shouldReturn` [toCardCode Locations.hiddenPath]

    it "builds the encounter deck from Flies and Spiders and Wilderland" . fsTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 38
      all ((`elem` map Just [FliesAndSpiders, Wilderland]) . cdEncounterSet . toCardDef) deck `shouldBe` True

    it "lets each player shuffle 1 copy of Bilbo's Magic Ring into his deck" . fsTest $ \pid -> do
      run Setup
      chooseLabel "$label.keepHand" pid
      _ <- setEncounterDeck [Locations.hiddenPath]
      chooseOptionMatching "flip the quest" pid (const True)
      chooseLabel "$label.intoMirkwood.shuffleIn" pid
      deck <- (.deck) <$> getPlayer pid
      length (filter ((== toCardCode Attachments.bilbosMagicRing) . toCardCode) deck) `shouldBe` 1

  -- \* Poison and unconscious characters

  describe "poison" do
    it "has an enemy with venom give a character it damages 1 poison from the top of its controller's deck" . fsTest $ \pid -> do
      spider <- engaged pid Enemies.wickedSpider
      b <- beravor
      deck <- deckSize pid
      _ <- setEncounterDeck [Locations.hiddenPath]
      run $ DealShadowCard spider
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      -- Wicked Spider's Forced effect: 1 poison to a character he controls
      chooseTarget pid b
      passActions pid
      -- Beravor defends, and takes 2 damage
      chooseTarget pid b
      passActions pid
      poison b `shouldReturn` 2
      deckSize pid `shouldReturn` (deck - 2)

    it "makes a character unconscious once its poison equals its printed hit points, the poison discarded" . fsTest $ \pid -> do
      b <- beravor
      discards <- discardSize pid
      run $ GivePoison b 3
      unconscious b `shouldReturn` False
      run $ GivePoison b 1
      unconscious b `shouldReturn` True
      exhausted b `shouldReturn` True
      poison b `shouldReturn` 0
      discardSize pid `shouldReturn` (discards + 4)

    it "counts poison as Condition attachments" . fsTest $ \_ -> do
      b <- beravor
      run $ GivePoison b 1
      conditions <- getConditionAttachments
      length conditions `shouldBe` 1

    it "keeps unconscious characters from questing, fighting and readying, but not from being readied as unconscious" . fsTest $ \pid -> do
      _ <- reachStage Quests.intoMirkwood
      b <- beravor
      run $ MakeUnconscious b
      mods <- getModifiers b
      mods `shouldSatisfy` \ms -> all (`elem` ms) [CannotCommitToQuest, CannotAttackOrDefend, CannotCollectResources, CannotTriggerAbilities, CannotBePoisoned, CannotReady]
      run $ Ready b
      exhausted b `shouldReturn` True
      deck <- deckSize pid
      run $ GivePoison b 1
      deckSize pid `shouldReturn` deck
      run $ ReadyUnconscious b
      unconscious b `shouldReturn` False
      exhausted b `shouldReturn` False

  -- \* The encounter cards

  describe "the enemies" do
    it "has Old Tomnoddy give each player's hero 1 poison at the end of the round" . fsTest $ \pid -> do
      _ <- stage Enemies.oldTomnoddy
      b <- beravor
      run $ CheckWindows [mkAfter RoundEnds]
      chooseTarget pid b
      poison b `shouldReturn` 1

    it "has Crazy Cob attack the character with the most poison, undefended damage going to it" . fsTest $ \pid -> do
      b <- beravor
      run $ GivePoison b 1
      -- the shadow card dealt it comes from below it in the deck
      _ <- setEncounterDeck [Enemies.crazyCob, Locations.hiddenPath]
      run RevealEncounterCard
      resolveRevealed
      cob <- enemyOf Enemies.crazyCob
      getModifiers (EnemyTarget cob) >>= (`shouldSatisfy` elem (UndefendedDamageTo b))
      -- undefended, Beravor takes the damage
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      getDamage b `shouldReturn` 3

    it "has Lazy Lob poison each character committed to the quest" . fsTest $ \pid -> do
      b <- beravor
      run $ CommitToQuest b
      reveal [Enemies.lazyLob]
      poison b `shouldReturn` 1
      e <- hero "01007"
      poison e `shouldReturn` 0
      _ <- pure pid
      pure ()

    it "gives Fat Spider +3 attack while engaged with a player who controls a poisoned character" . fsTest $ \pid -> do
      fat <- engaged pid Enemies.fatSpider
      getEnemyAttack fat `shouldReturn` 1
      b <- beravor
      run $ GivePoison b 1
      getEnemyAttack fat `shouldReturn` 4

    it "has Dark Bats' shadow attach it to the defending character, -1 to each of its stats" . fsTest $ \pid -> do
      spider <- engaged pid Enemies.wickedSpider
      b <- beravor
      willpower <- getWillpower b
      shadowAttack pid spider Enemies.darkBats
      -- Wicked Spider's poison, then Beravor defends
      chooseTarget pid b
      passActions pid
      chooseTarget pid b
      passActions pid
      bats <- enemyOf Enemies.darkBats
      (.placement) <$> getsGame (fromJustNote "bats" . Map.lookup bats . (.entities.enemies)) `shouldReturn` AttachedTo b
      getWillpower b `shouldReturn` (willpower - 1)

  describe "the locations" do
    it "has Giant Web, active, take 1 from each stat per poison, and ready an unconscious character as it is explored" . fsTest $ \pid -> do
      b <- beravor
      run $ GivePoison b 2
      defense <- getDefense b
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.giantWeb
      getDefense b `shouldReturn` (defense - 2)
      t <- hero "01002"
      run $ MakeUnconscious t
      web <- locationOf Locations.giantWeb
      run $ PlaceTokens GameSource (LocationTarget web) Progress 3
      chooseAbilityOf pid (LocationTarget web)
      unconscious t `shouldReturn` False

    it "keeps poisoned characters from questing at Forest Stream, and discards 4 poison as it is explored" . fsTest $ \pid -> do
      b <- beravor
      run $ GivePoison b 2
      t <- hero "01002"
      run $ GivePoison t 3
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.forestStream
      getModifiers b >>= (`shouldSatisfy` elem CannotCommitToQuest)
      stream <- locationOf Locations.forestStream
      run $ PlaceTokens GameSource (LocationTarget stream) Progress 4
      chooseAbilityOf pid (LocationTarget stream)
      -- Beravor's 2, then Théodred, the only one left, loses 2 of his 3
      replicateM_ 2 $ chooseTarget pid b
      poison b `shouldReturn` 0
      poison t `shouldReturn` 1

    it "has The Spider's Glade cost an exhausted Bilbo to travel, and bring a Spider each quest phase" . fsTest $ \pid -> do
      setUp pid
      run . AddToStaging =<< genEncounterCard Locations.theSpidersGlade
      glade <- locationOf Locations.theSpidersGlade
      getModifiers (LocationTarget glade) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      _ <- setEncounterDeck [Locations.giantWeb, Enemies.wickedSpider]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      enemyCodes `shouldReturn` [toCardCode Enemies.wickedSpider]
      run $ TravelTo glade
      b <- bilbo
      exhausted b `shouldReturn` True
      _ <- pure pid
      pure ()

    it "has Hidden Path cost 2 Baggins resources to travel to, and lower the staged locations' threat" . fsTest $ \pid -> do
      setUp pid
      path <- locationOf Locations.hiddenPath
      getModifiers (LocationTarget path) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      b <- bilbo
      run $ PlaceTokens GameSource b Resource 2
      getModifiers (LocationTarget path) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run . AddToStaging =<< genEncounterCard Locations.giantWeb
      web <- locationOf Locations.giantWeb
      run $ TravelTo path
      replicateM_ 2 $ chooseTarget pid b
      getHeroResources (case b of HeroTarget h -> h; _ -> error "bilbo") `shouldReturn` 0
      getLocationThreat web `shouldReturn` 2
      _ <- pure (pid, Baggins)
      pure ()

  describe "the treacheries" do
    it "has Caught in Webs remove a committed hero from the quest and make it unconscious, surging with none" . fsTest $ \_ -> do
      b <- beravor
      run $ CommitToQuest b
      -- the only hero committed
      reveal [Treacheries.caughtInWebs]
      unconscious b `shouldReturn` True
      (heroCommitted . toAttrs <$> getHeroWithCode "01012") `shouldReturn` False
      -- none committed: it surges
      _ <- setEncounterDeck [Treacheries.caughtInWebs, Locations.giantWeb]
      run RevealEncounterCard
      resolveRevealed
      locationCodes `shouldReturn` [toCardCode Locations.giantWeb]

    it "has Poisoned by Spiders deal each character its poison in damage, surging with none damaged" . fsTest $ \_ -> do
      b <- beravor
      run $ GivePoison b 2
      reveal [Treacheries.poisonedBySpiders]
      getDamage b `shouldReturn` 2
      run $ GivePoison b 0
      _ <- setEncounterDeck [Treacheries.poisonedBySpiders, Locations.giantWeb]
      g <- getGame
      putGame $ g & entitiesL . heroesL . traverse %~ overAttrs (\h -> h {heroPoison = []})
      run RevealEncounterCard
      resolveRevealed
      locationCodes `shouldReturn` [toCardCode Locations.giantWeb]

    it "has Fighting Among Friends remove the committed characters outside the named sphere" . fsTest $ \pid -> do
      b <- beravor
      t <- hero "01002"
      run $ CommitToQuest b
      run $ CommitToQuest t
      reveal [Treacheries.fightingAmongFriends]
      -- Beravor is Lore, Théodred Leadership
      chooseLabel "$label.fightingAmongFriends.lore" pid
      (heroCommitted . toAttrs <$> getHeroWithCode "01012") `shouldReturn` True
      (heroCommitted . toAttrs <$> getHeroWithCode "01002") `shouldReturn` False

    it "has Weighed Down take 1 from each stat of each exhausted character until the end of the round" . fsTest $ \_ -> do
      b <- beravor
      attack <- getAttack b
      reveal [Treacheries.weighedDownWilderland]
      getAttack b `shouldReturn` attack
      run $ Exhaust b
      getAttack b `shouldReturn` (attack - 1)

  -- \* The stages

  describe "the stages" do
    it "has Lost in the Dark, alone, discard the staging area and go on to Captured by Spiders" . fsTest $ \pid -> do
      setQuestDeck [Quests.lostInTheDarkFliesAndSpiders, Quests.capturedBySpiders, Quests.battleWithTheSpiders]
      _ <- putQuestStage Quests.intoMirkwood
      _ <- stage Enemies.fatSpider
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 9
      resolveRevealed
      enemyCodes `shouldReturn` []
      questCodes `shouldReturn` [toCardCode Quests.capturedBySpiders]
      questDeck `shouldReturn` [toCardCode Quests.battleWithTheSpiders]
      -- Captured by Spiders: each character but Bilbo is unconscious
      b <- beravor
      unconscious b `shouldReturn` True
      _ <- pure pid
      pure ()

    it "has Lost in the Dark give the first player his own staging area at stage 3, the others going on to stage 4" . fsTwoPlayerTest $ \one two -> do
      setQuestDeck [Quests.lostInTheDarkFliesAndSpiders, Quests.capturedBySpiders, Quests.battleWithTheSpiders]
      run . SetAside =<< genEncounterCard Locations.theSpidersGlade
      _ <- putQuestStage Quests.intoMirkwood
      _ <- stage Enemies.fatSpider
      lead <- getLead
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 9
      resolveRevealed
      let other = if lead == one then two else one
      areas `shouldReturn` [[lead], [other]]
      questCodes `shouldReturn` [toCardCode Quests.capturedBySpiders]
      enemyCodes `shouldReturn` []
      run $ FocusArea other
      resolveRevealed
      questCodes `shouldReturn` [toCardCode Quests.battleWithTheSpiders]
      sort <$> ((<>) <$> enemyCodes <*> locationCodes) `shouldReturn` sort [toCardCode Enemies.fatSpider, toCardCode Locations.theSpidersGlade]

    it "has Captured by Spiders, alone, keep the first player token and advance only at the end of the quest phase" . fsTest $ \pid -> do
      setQuestDeck [Quests.battleWithTheSpiders]
      q <- reachStage Quests.capturedBySpiders
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotLoseFirstPlayerToken)
      run $ PlaceTokens GameSource q Progress 11
      questCodes `shouldReturn` [toCardCode Quests.capturedBySpiders]
      run $ EndPhase QuestPhase
      resolveRevealed
      questCodes `shouldReturn` [toCardCode Quests.battleWithTheSpiders]

    it "lets a player spend 2 Baggins resources to ready an unconscious character he controls" . fsTest $ \pid -> do
      setUp pid
      _ <- reachStage Quests.capturedBySpiders
      b <- beravor
      run $ MakeUnconscious b
      bh <- getHeroWithCode "132001"
      run $ PlaceTokens GameSource (toTarget bh) Resource 2
      run $ BeginPhase CombatPhase
      quest <- getsGame (toTarget . fromJustNote "no stage 3" . find ((== toCardCode Quests.capturedBySpiders) . toCardCode) . toList . (.entities.quests))
      chooseAbilityOf pid quest
      -- each character but Bilbo is unconscious since stage 3A: the player picks Beravor
      chooseTarget pid b
      unconscious b `shouldReturn` False
      getHeroResources bh.id `shouldReturn` 0

    it "keeps the first player token with the first player apart at Captured by Spiders" . fsTwoPlayerTest $ \one two -> do
      run $ SeparatePlayers [[two], [one]]
      run $ FocusArea one
      _ <- reachStage Quests.capturedBySpiders
      run $ FocusArea two
      _ <- reachStage Quests.battleWithTheSpiders
      run $ FocusArea one
      run $ BeginPhase RefreshPhase
      passActions one
      run $ EndPhase RefreshPhase
      passActions two
      getsGame (fmap separationFirstPlayer . gameSeparation) `shouldReturn` Just one

    it "joins the first player to the others at the end of the quest phase once Captured by Spiders is complete" . fsTwoPlayerTest $ \one two -> do
      run $ SeparatePlayers [[two], [one]]
      run $ FocusArea one
      q <- reachStage Quests.capturedBySpiders
      _ <- stage Enemies.fatSpider
      run $ PlaceTokens GameSource q Progress 11
      questCodes `shouldReturn` [toCardCode Quests.capturedBySpiders]
      run $ FocusArea two
      _ <- reachStage Quests.battleWithTheSpiders
      run JoinAreas
      areas `shouldReturn` []
      questCodes `shouldReturn` [toCardCode Quests.battleWithTheSpiders]
      enemyCodes `shouldReturn` [toCardCode Enemies.fatSpider]

    it "takes no progress on Battle with the Spiders while The Spider's Glade is in play, and wins once defeated" . fsTest $ \_ -> do
      run . SetAside =<< genEncounterCard Locations.theSpidersGlade
      q <- reachStage Quests.battleWithTheSpiders
      resolveRevealed
      locationCodes `shouldReturn` [toCardCode Locations.theSpidersGlade]
      getModifiers q >>= (`shouldSatisfy` elem (ProgressLimit 0))
      glade <- locationOf Locations.theSpidersGlade
      run $ Discard (LocationTarget glade)
      run $ PlaceTokens GameSource q Progress 3
      getsGame (.outcome) `shouldReturn` Just Won

  describe "campaign mode" do
    it "records Bilbo's Magic Ring as discovered by each player once won" . fsTest $ \_ -> do
      let l = newCampaignLog "the-hobbit"
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioCampaign = Just l})
      run ResolveCampaign
      pool <- (.pool) <$> getCampaignLog
      [(p.code, p.seat) | p <- pool] `shouldBe` [(toCardCode Attachments.bilbosMagicRing, Just 0)]
