module LordOfTheRings.EredMithrin.JourneyUpTheAnduinSpec (spec) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCard, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (Wilderlands))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (AllyMatcher (..), PlayerMatcher (..), getEnemy, select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (EncounterPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (journeyUpTheAnduin, lookupScenario)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), evilCreaturesDeckL, modeL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

anduinTest :: (PlayerId -> GameT ()) -> IO ()
anduinTest = gameTestIn journeyUpTheAnduin testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | The Evil Creatures deck, top first, as the given cards.
setEvilCreatures :: [CardDef] -> GameT [EncounterCard]
setEvilCreatures defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (evilCreaturesDeckL ?~ cards)
  pure cards

setSetAside :: [CardDef] -> GameT ()
setSetAside defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

enemyWithCard :: EncounterCard -> GameT EnemyId
enemyWithCard card = getsGame (fromJustNote "not in play" . fmap toId . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  enemyWithCard card

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

questResources :: GameT Int
questResources = countTokens Resource . questTokens . toAttrs <$> getActiveQuest

flipped :: CardDef -> GameT Target
flipped def = do
  q <- putQuestStage def
  run $ Flip q
  resolveRevealed
  pure q

spec :: Spec
spec = describe "Journey Up the Anduin" do
  it "is a scenario the game can be dealt" . anduinTest $ \_ ->
    isJust (lookupScenario "journey-up-the-anduin") `shouldBe` True

  describe "setup" do
    it "makes the Evil Creatures deck of Wilderlands' enemies, removes its other cards, sets Woodmen Village and The Old Ford aside, and stages a location" . anduinTest $ \pid -> do
      flipFirstQuest pid
      a <- scenarioAttrs
      let evil = fromMaybe [] a.evilCreaturesDeck
          fromWilderlands = (== Just Wilderlands) . cdEncounterSet . toCardDef
      length evil `shouldBe` 8
      all (\c -> fromWilderlands c && cdCardType (toCardDef c) == EnemyType) evil `shouldBe` True
      filter fromWilderlands (a.encounterDeck <> a.encounterDiscard) `shouldBe` []
      sort (map toCardCode a.scenarioSetAside) `shouldBe` sort (map toCardCode [Locations.woodmenVillage, Locations.theOldFordJourneyUpTheAnduin])
      staged <- getsGame (map (.placement) . toList . (.entities.locations))
      staged `shouldBe` [StagingArea]
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.travelingNorth

    it "leaves the Wilderlands enemies easy mode removes out of the Evil Creatures deck" . gameTestIn (overAttrs (modeL .~ EasyMode) journeyUpTheAnduin) testDeck $ \pid -> do
      flipFirstQuest pid
      evil <- map toCardCode . fromMaybe [] . (.evilCreaturesDeck) <$> scenarioAttrs
      length evil `shouldBe` 6
      map (\d -> length (filter (== toCardCode d) evil)) [Enemies.hillTroll, Enemies.marshAdder] `shouldBe` [1, 0]

  describe "the Evil Creatures deck" do
    it "takes its defeated enemies, victory points and all, into its own discard pile" . anduinTest $ \_ -> do
      _ <- setEvilCreatures [Enemies.wargs]
      troll <- stage Enemies.hillTroll
      run $ Destroy GameSource (EnemyTarget troll)
      a <- scenarioAttrs
      (map toCardCode a.scenarioEvilCreaturesDiscard, a.scenarioVictoryDisplay) `shouldBe` ([toCardCode Enemies.hillTroll], [])

    it "takes back its discard pile once it is empty" . anduinTest $ \_ -> do
      _ <- setEvilCreatures [Enemies.wargs]
      troll <- stage Enemies.hillTroll
      run $ Destroy GameSource (EnemyTarget troll)
      run $ AddFromEvilCreaturesDeck GameSource Nothing
      a <- scenarioAttrs
      (map toCardCode <$> a.evilCreaturesDeck, a.scenarioEvilCreaturesDiscard) `shouldBe` (Just [toCardCode Enemies.hillTroll], [])
      enemyCodes `shouldReturn` [toCardCode Enemies.wargs]

  describe "stages" do
    it "has stage 1B gain a resource from questing and spend it to add an Evil Creature" . anduinTest $ \pid -> do
      q <- flipped Quests.travelingNorth
      [wargs] <- setEvilCreatures [Enemies.wargs]
      run $ CheckWindows [mkAfter PlacedProgressFromQuesting]
      questResources `shouldReturn` 1
      run $ CheckWindows [mkAfter $ PhaseBegins EncounterPhase]
      questResources `shouldReturn` 0
      _ <- enemyWithCard wargs
      -- with no resource to remove, nothing is added (FAQ 1.15)
      _ <- setEvilCreatures [Enemies.hillTroll]
      run $ CheckWindows [mkAfter $ PhaseBegins EncounterPhase]
      enemyCodes `shouldReturn` [toCardCode Enemies.wargs]
      getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
      offersAbilityOf pid q `shouldReturn` False

    it "has stage 2A add Woodmen Village to the staging area" . anduinTest $ \pid -> do
      setSetAside [Locations.woodmenVillage]
      q <- putQuestStage Quests.woodmenUnderAttack
      run $ Flip q
      resolveWhenRevealed pid
      _ <- locationOf Locations.woodmenVillage
      pure ()

    it "has stage 2B limit staged locations' progress, add an Evil Creature on travel, and wait for the first player to control Haldan" . anduinTest $ \_ -> do
      q <- flipped Quests.woodmenUnderAttack
      lid <- stageLocation Locations.woodedRiverbank
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem (ProgressPerRoundLimit 1))
      getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
      _ <- setEvilCreatures [Enemies.wargs]
      run $ TravelTo lid
      enemyCodes `shouldReturn` [toCardCode Enemies.wargs]

    it "has Woodmen Village turn into Haldan, guarded by the Evil Creature it adds, who comes to the first player as it is defeated" . anduinTest $ \pid -> do
      q <- flipped Quests.woodmenUnderAttack
      [troll] <- setEvilCreatures [Enemies.hillTroll]
      lid <- stageLocation Locations.woodmenVillage
      run $ PlaceLocation lid ActiveLocation
      village <- getsGame (fmap toCard . lookup lid . (.entities.locations))
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
      eid <- enemyWithCard troll
      getsGame (lookup lid . (.entities.locations)) >>= (`shouldSatisfy` isNothing)
      a <- scenarioAttrs
      map (.id) a.encounterDiscard `shouldBe` []
      -- a captive, out of every matcher's reach, under Woodmen Village's card id
      captives <- getsGame (map toAttrs . toList . (.entities.allies))
      map (\h -> (toCardCode h, Just (allyCardId h), h.placement)) captives
        `shouldBe` [(toCardCode Objectives.haldan, (.id) <$> village, AttachedTo (CardIdTarget troll.id))]
      select (AllyControlledBy $ PlayerWithId pid) `shouldReturn` []
      run $ Destroy GameSource (EnemyTarget eid)
      haldan <- select (AllyControlledBy (PlayerWithId pid) <> AllyWithCardCode (toCardCode Objectives.haldan))
      length haldan `shouldBe` 1
      getModifiers q >>= (`shouldSatisfy` notElem CannotBeDefeated)

    it "has stage 3A add The Old Ford, and stage 3B bar traveling there and defeating it, add an Evil Creature each quest phase, and win as The Old Ford is explored" . anduinTest $ \pid -> do
      setSetAside [Locations.theOldFordJourneyUpTheAnduin]
      q <- putQuestStage Quests.thePassageOfTheFord
      run $ Flip q
      resolveWhenRevealed pid
      ford <- locationOf Locations.theOldFordJourneyUpTheAnduin
      getModifiers (LocationTarget ford) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
      run $ PlaceTokens GameSource q Progress 5
      getModifiers (LocationTarget ford) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      _ <- setEvilCreatures [Enemies.wargs]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      enemyCodes `shouldReturn` [toCardCode Enemies.wargs]
      run $ PlaceTokens GameSource (LocationTarget ford) Progress 10
      getsGame (.outcome) `shouldReturn` Just Won

  describe "encounter cards" do
    it "gives The Old Ford 5 quest points for each enemy, and has the staged enemies attack the first player as it becomes active" . anduinTest $ \pid -> do
      ford <- stageLocation Locations.theOldFordJourneyUpTheAnduin
      wargs <- stage Enemies.wargs
      getModifiers (LocationTarget ford) >>= (`shouldSatisfy` elem (QuestPointModifier 5))
      getModifiers (LocationTarget ford) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ TravelTo ford
      passActions pid
      Just (QuestionLabel _ source _ _) <- getQuestion pid
      source `shouldBe` Just (EnemyTarget wargs)

    it "has Wooded Riverbank put an Evil Creature into play engaged with the first player as it is explored" . anduinTest $ \pid -> do
      [wargs] <- setEvilCreatures [Enemies.wargs]
      lid <- stageLocation Locations.woodedRiverbank
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
      eid <- enemyWithCard wargs
      (.placement) <$> getEnemy eid `shouldReturn` EngagedWith pid

    it "has Frenzied Creature add an Evil Creature and attach to it" . anduinTest $ \_ -> do
      [wargs] <- setEvilCreatures [Enemies.wargs]
      _ <- setEncounterDeck [Treacheries.frenziedCreature]
      run RevealEncounterCard
      resolveRevealed
      eid <- enemyWithCard wargs
      getEnemyThreat eid `shouldReturn` 3
      getEnemyAttack eid `shouldReturn` 4
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

    it "has Dangerous Crossing surge with less than 3 threat at the active location" . anduinTest $ \_ -> do
      _ <- setEncounterDeck [Treacheries.dangerousCrossing, Enemies.wargs]
      run RevealEncounterCard
      resolveRevealed
      enemyCodes `shouldReturn` [toCardCode Enemies.wargs]

    it "has Dangerous Crossing have each player assign the active location's threat in damage" . anduinTest $ \pid -> do
      lid <- stageLocation Locations.woodedRiverbank
      run $ PlaceLocation lid ActiveLocation
      _ <- setEncounterDeck [Treacheries.dangerousCrossing, Enemies.wargs]
      run RevealEncounterCard
      resolveRevealed
      h <- heroOf pid "01012"
      replicateM_ 3 $ chooseTarget pid h
      getDamage h `shouldReturn` 3
      enemyCodes `shouldReturn` []
