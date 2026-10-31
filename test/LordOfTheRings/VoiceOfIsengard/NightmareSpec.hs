module LordOfTheRings.VoiceOfIsengard.NightmareSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (PlayerWithCardsInHand), getEnemy, getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (intoFangorn, questDeckL, theFordsOfIsen, toCatchAnOrc)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (outOfPlayDeckOf)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (scenarioSetAside), metaL, modeL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (AdvancedToStage, PhaseBegins), mkAfter)
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

builtDeck :: GameT [EncounterCard]
builtDeck = run (DoStep 1 Setup) >> getEncounterDeck

reveal :: [CardDef] -> GameT ()
reveal defs = setEncounterDeck defs >> run RevealEncounterCard >> resolveRevealed

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure $ headMay $ [l.placement | l <- toList g.entities.locations, toCardCode l == code] <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  stage def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

shadowCount :: EnemyId -> GameT Int
shadowCount eid = length . (.shadowCards) <$> getEnemy eid

-- | Cards put into the hand, the modifiers read again after.
fillHand :: PlayerId -> Int -> GameT ()
fillHand pid n = do
  replicateM_ n $ putCardIntoHand pid Enemies.forestSpider
  run $ CheckWindows []

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

explore :: CardDef -> Int -> GameT ()
explore def progress = do
  lid <- locationOf def
  run $ PlaceLocation lid ActiveLocation
  run $ PlaceTokens GameSource (toTarget lid) Progress progress

setAsideCodes :: GameT [CardCode]
setAsideCodes = getsGame (sort . map toCardCode . scenarioSetAside . toAttrs . (.scenario))

-- | The player's out-of-play deck is set to the cards, top first.
setOutOfPlayDeck :: PlayerId -> [Card] -> GameT ()
setOutOfPlayDeck pid cards = do
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (metaL .~ toJSON (Map.singleton pid cards))

playerCard :: PlayerId -> CardDef -> GameT Card
playerCard pid def = PlayerCard <$> genPlayerCard def (Just pid)

-- | Mugash, the Into Fangorn objective, attached to Théodred.
mugashOnTheodred :: GameT Target
mugashOnTheodred = do
  run . AddToStaging =<< genEncounterCard Objectives.mugashObjective
  theodred <- toTarget <$> getHeroWithCode "01002"
  oid <- getsGame (fromJustNote "no Mugash" . fmap toId . find ((== toCardCode Objectives.mugashObjective) . toCardCode) . toList . (.entities.objectives))
  run $ ClaimObjective oid theodred
  pure theodred

spec :: Spec
spec = describe "The Voice of Isengard Nightmare Decks" do
  describe "The Fords of Isen" do
    it "builds the nightmare deck: removals out, the nightmare set in" . nightmare theFordsOfIsen $ \_ -> do
      deck <- builtDeck
      map (`copies` deck) [Enemies.dunlandProwler, Enemies.dunlendingBandit, Enemies.dunlandBerserker, Locations.theKingsRoad, Locations.gapOfRohan]
        `shouldBe` [0, 0, 2, 0, 2]
      map (`copies` deck) [Enemies.dunlandAssassin, Enemies.savageHillman, Locations.westernShore, Treacheries.brutalAssault]
        `shouldBe` [2, 3, 3, 2]
      copies Nightmare.theFordsOfIsenNightmare deck `shouldBe` 0

    it "has each player with fewer than 3 cards draw 3 as the players advance" . nightmare theFordsOfIsen $ \pid -> do
      run $ CheckWindows [mkAfter AdvancedToStage]
      handSize pid `shouldReturn` 3
      run $ CheckWindows [mkAfter AdvancedToStage]
      handSize pid `shouldReturn` 3

    it "Dunland Assassin attacks the first player as he draws, from the staging area" . nightmare theFordsOfIsen $ \pid -> do
      stage Enemies.dunlandAssassin
      eid <- enemyOf Enemies.dunlandAssassin
      getModifiers eid >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy (PlayerWithCardsInHand 3)))
      _ <- setEncounterDeck [Locations.isenRiver]
      run $ DrawCards (Drawing pid 1)
      shadowCount eid `shouldReturn` 1

    it "Savage Hillman takes no damage with 3 cards in hand, and gets +2 attack with 5" . nightmare theFordsOfIsen $ \pid -> do
      eid <- engaged pid Enemies.savageHillman
      fillHand pid 3
      run $ DealDamage GameSource (toTarget eid) 2
      (.damage) <$> getEnemy eid `shouldReturn` 0
      getEnemyAttack eid `shouldReturn` 2
      fillHand pid 2
      getEnemyAttack eid `shouldReturn` 4

    it "Western Shore's threat is the first player's hand, taken off each enemy's engagement cost" . nightmare theFordsOfIsen $ \pid -> do
      stage Locations.westernShore
      stage Enemies.dunlandAssassin
      fillHand pid 4
      lid <- locationOf Locations.westernShore
      getLocationThreat lid `shouldReturn` 4
      (getEngagementCost =<< enemyOf Enemies.dunlandAssassin) `shouldReturn` 41

    it "Isen River keeps the players from discarding from hand" . nightmare theFordsOfIsen $ \pid -> do
      stage Locations.isenRiver
      card <- putCardIntoHand pid Enemies.forestSpider
      run $ DiscardFromHand pid card.id
      handSize pid `shouldReturn` 1

    it "Brutal Assault has each player assign damage equal to his hand" . nightmare theFordsOfIsen $ \pid -> do
      fillHand pid 2
      reveal [Treacheries.brutalAssault]
      hero <- heroOf pid "01012"
      chooseTarget pid hero
      chooseTarget pid hero
      getCharacterDamage hero `shouldReturn` 2

  describe "To Catch an Orc" do
    it "builds the nightmare deck, the Vigilant Orcs and Mugash's Rage set aside" . nightmare toCatchAnOrc $ \_ -> do
      deck <- builtDeck
      map (`copies` deck) [Enemies.prowlingWolf, Enemies.orcSkirmisher, Treacheries.orcHuntingParty, Locations.methedras, Locations.orcCave]
        `shouldBe` [0, 2, 0, 2, 2]
      map (`copies` deck) [Enemies.vigilantOrc, Treacheries.mugashsRage, Enemies.guardWarg]
        `shouldBe` [0, 0, 2]
      setAsideCodes
        `shouldReturn` sort
          (replicate 4 (toCardCode Enemies.vigilantOrc) <> replicate 2 (toCardCode Treacheries.mugashsRage) <> [toCardCode Enemies.mugashEnemy] <> replicate 3 (toCardCode Enemies.mugashsGuard))

    it "shuffles a Vigilant Orc into each out-of-play deck" . nightmare toCatchAnOrc $ \_ -> do
      _ <- builtDeck
      run $ ScenarioSpecific "outOfPlay.setup" Null
      lead <- getLead
      outOfPlay <- outOfPlayDeckOf lead
      sort [toCardCode c | c@(EncounterCard _) <- outOfPlay] `shouldBe` sort [toCardCode Enemies.mugashEnemy, toCardCode Enemies.vigilantOrc]
      setAsideCodes
        `shouldReturn` sort (replicate 3 (toCardCode Enemies.vigilantOrc) <> replicate 2 (toCardCode Treacheries.mugashsRage) <> replicate 3 (toCardCode Enemies.mugashsGuard))

    it "shuffles Mugash's Rage into the encounter deck as the players reach stage 3" . nightmare toCatchAnOrc $ \_ -> do
      _ <- builtDeck
      _ <- putQuestStage Quests.theWizardsPrize
      run $ CheckWindows [mkAfter AdvancedToStage]
      copies Treacheries.mugashsRage <$> getEncounterDeck `shouldReturn` 2

    it "Vigilant Orc attacks the first player as it enters the staging area" . nightmare toCatchAnOrc $ \_ -> do
      _ <- setEncounterDeck [Locations.orcTrail]
      stage Enemies.vigilantOrc
      (shadowCount =<< enemyOf Enemies.vigilantOrc) `shouldReturn` 1

    it "Mugash's Cunning stages the enemies it discards from the out-of-play decks, and Guard Warg attacks" . nightmare toCatchAnOrc $ \pid -> do
      warg <- engaged pid Enemies.guardWarg
      a <- playerCard pid Allies.snowbournScout
      orc <- EncounterCard <$> genEncounterCard Enemies.methedrasOrc
      setOutOfPlayDeck pid [orc, a]
      reveal [Treacheries.mugashsCunning, Locations.orcTrail, Locations.orcTrail]
      placementOf Enemies.methedrasOrc `shouldReturn` Just StagingArea
      elem a.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` True
      -- no surge: the location under it stays
      placementOf Locations.orcTrail `shouldReturn` Nothing
      shadowCount warg >>= (`shouldSatisfy` (> 0))

    it "Mugash's Cunning surges when it stages no enemy" . nightmare toCatchAnOrc $ \pid -> do
      a <- playerCard pid Allies.snowbournScout
      setOutOfPlayDeck pid [a]
      reveal [Treacheries.mugashsCunning, Locations.orcTrail]
      placementOf Locations.orcTrail `shouldReturn` Just StagingArea

    it "Patrolling Orcs makes the engaging player exhaust a character or lose a time counter" . nightmare toCatchAnOrc $ \pid -> do
      q <- putQuestStage Quests.searchingForMugash
      run $ PlaceTokens GameSource q TimeCounter 2
      reveal [Treacheries.patrollingOrcs, Locations.orcTrail]
      _ <- engaged pid Enemies.guardWarg
      chooseLabel "$label.patrollingOrcs.timeCounter" pid
      getTimeCounters q `shouldReturn` 1

    it "Methedras Peak's threat is the first player's characters" . nightmare toCatchAnOrc $ \_ -> do
      stage Locations.methedrasPeak
      (getLocationThreat =<< locationOf Locations.methedrasPeak) `shouldReturn` 3

    it "Orc Trail stages an out-of-play Orc once explored" . nightmare toCatchAnOrc $ \_ -> do
      run . SetAside =<< genEncounterCard Enemies.methedrasOrc
      stage Locations.orcTrail
      explore Locations.orcTrail 2
      placementOf Enemies.methedrasOrc `shouldReturn` Just StagingArea

  describe "Into Fangorn" do
    it "builds the nightmare deck" . nightmare intoFangorn $ \_ -> do
      deck <- builtDeck
      map (`copies` deck) [Locations.tangledWoods, Treacheries.turnedAround, Enemies.deadlyHuorn, Enemies.huornOfFangorn, Locations.entwood]
        `shouldBe` [0, 0, 0, 3, 3]

    it "has each player put a Huorn into play engaged with him during setup" . nightmare intoFangorn $ \pid -> do
      _ <- setEncounterDeck [Locations.entwood, Enemies.wickedHuorn]
      run $ DoStep 8 Setup
      chooseOptionMatching "choose the Huorn" pid \case
        TargetLabel _ _ -> True
        _ -> False
      placementOf Enemies.wickedHuorn `shouldReturn` Just (EngagedWith pid)

    it "does not advance Into the Woods until Edge of Fangorn is in the victory display" . nightmare intoFangorn $ \pid -> do
      q <- putQuestStage Quests.intoTheWoods
      cards <- traverse genEncounterCard [Quests.escapeFromFangorn]
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)
      _ <- mugashOnTheodred
      run $ PlaceTokens GameSource q Progress 9
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.intoTheWoods
      edge <- EncounterCard <$> genEncounterCard Locations.edgeOfFangorn
      g' <- getGame
      putGame $ g' & scenarioL %~ overAttrs (victoryDisplayL .~ [edge])
      run $ PlaceTokens GameSource q Progress 1
      resolveWhenRevealed pid
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.escapeFromFangorn

    it "Blocking Huorn adds its threat to the staging area while engaged" . nightmare intoFangorn $ \pid -> do
      _ <- engaged pid Enemies.blockingHuorn
      getStagingThreat `shouldReturn` 2

    it "Wicked Huorn exhausts a hero of the engaged player as the resource phase begins" . nightmare intoFangorn $ \pid -> do
      _ <- engaged pid Enemies.wickedHuorn
      run $ CheckWindows [mkAfter $ PhaseBegins ResourcePhase]
      hero <- heroOf pid "01012"
      chooseTarget pid hero
      getsGame (any (heroExhausted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` True

    it "Dark Corner of Fangorn takes hinder from the Huorns while staged" . nightmare intoFangorn $ \pid -> do
      q <- putQuestStage Quests.escapeFromFangorn
      run $ PlaceTokens GameSource q Progress 2
      _ <- engaged pid Enemies.huornOfFangorn
      stage Locations.darkCornerOfFangorn
      _ <- setEncounterDeck [Treacheries.stiflingWoods]
      run $ BeginPhase CombatPhase
      (.progress) <$> getActiveQuest `shouldReturn` 2

    it "Entwood makes the staged Huorns immune to player cards" . nightmare intoFangorn $ \_ -> do
      stage Locations.entwood
      stage Enemies.huornOfFangorn
      (getModifiers =<< enemyOf Enemies.huornOfFangorn) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

    it "Tangled Forest turns progress removed from the quest into damage" . nightmare intoFangorn $ \pid -> do
      q <- putQuestStage Quests.escapeFromFangorn
      run $ PlaceTokens GameSource q Progress 2
      stage Locations.tangledForest
      stage Locations.tangledForest
      run $ RemoveProgressFromQuest GameSource 5
      hero <- heroOf pid "01012"
      chooseTarget pid hero
      chooseTarget pid hero
      getCharacterDamage hero `shouldReturn` 2
      getQuestion pid `shouldReturn` Nothing

    it "Stifling Woods removes 6 progress from the quest, the first player's choice" . nightmare intoFangorn $ \pid -> do
      q <- putQuestStage Quests.escapeFromFangorn
      run $ PlaceTokens GameSource q Progress 7
      reveal [Treacheries.stiflingWoods]
      chooseLabel "$label.stiflingWoods.progress" pid
      (.progress) <$> getActiveQuest `shouldReturn` 1

    it "Fangorn's Anger damages the hero holding Mugash" . nightmare intoFangorn $ \_ -> do
      theodred <- mugashOnTheodred
      reveal [Treacheries.fangornsAnger]
      getCharacterDamage theodred `shouldReturn` 2

    it "Fangorn's Anger surges and dooms without Mugash on a hero" . nightmare intoFangorn $ \pid -> do
      was <- (.threat) <$> getPlayer pid
      reveal [Treacheries.fangornsAnger, Locations.entwood]
      (.threat) <$> getPlayer pid `shouldReturn` was + 2
      placementOf Locations.entwood `shouldReturn` Just StagingArea
