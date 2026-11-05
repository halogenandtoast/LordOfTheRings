module LordOfTheRings.VengeanceOfMordor.TheTempleOfDoomSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdEncounterSet), CardType (QuestType), cdCardType, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (ThePowerOfMordor))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (enemiesL, questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theTempleOfDoom)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, powerOfMordorDeckL, questDeckL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins, PhaseEnds, RoundEnds), mkAfter, mkWhen)
import TestImport

templeTest :: (PlayerId -> GameT ()) -> IO ()
templeTest = gameTestIn theTempleOfDoom testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

-- | Tribute to Mordor in the staging area.
stageTribute :: GameT Target
stageTribute = do
  run . AddToStaging =<< genEncounterCard Quests.tributeToMordor
  tribute

tribute :: GameT Target
tribute = getsGame (toTarget . fromJustNote "no Tribute to Mordor" . find ((== toCardCode Quests.tributeToMordor) . toCardCode) . toList . (.entities.quests))

-- | The main quest in play, whichever side quests are beside it.
mainQuest :: GameT Quest
mainQuest = getsGame (fromJustNote "no main quest" . find ((== QuestType) . cdCardType . toCardDef) . toList . (.entities.quests))

-- | A stage put into play on its B side.
stageB :: CardDef -> GameT Target
stageB def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

setQuestDeck :: [CardDef] -> GameT [EncounterCard]
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)
  pure cards

setPowerOfMordorDeck :: [CardDef] -> GameT ()
setPowerOfMordorDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (powerOfMordorDeckL .~ cards)

-- | These cards in the victory display.
inVictoryDisplay :: [CardDef] -> GameT ()
inVictoryDisplay defs = do
  cards <- traverse genEncounterCard defs
  g <- (scenarioL %~ overAttrs (victoryDisplayL .~ map EncounterCard cards)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

-- | N quest stages in the victory display.
stagesInVictory :: Int -> GameT ()
stagesInVictory n = inVictoryDisplay (replicate n Quests.theLegacyOfMordor)

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

-- | The enemy's damage set straight, as taken over earlier rounds.
withDamage :: EnemyId -> Int -> GameT ()
withDamage eid n = do
  g <- getGame
  let g' = g & entitiesL . enemiesL . ix eid %~ overAttrs (\a -> a {enemyTokens = addTokens Damage n a.enemyTokens})
  putGame g'
  putGame =<< preloadModifiers g'

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The enemy, engaged with the player, attacks him, the shadow card dealt to it first if
-- any, and the defender declares against it.
attackedBy :: PlayerId -> EnemyId -> [CardDef] -> Target -> GameT ()
attackedBy pid eid shadow defender = do
  unless (null shadow) do
    _ <- setEncounterDeck shadow
    run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

copiesOf :: CardDef -> [EncounterCard] -> Int
copiesOf def = length . filter ((== toCardCode def) . toCardCode)

stage2s :: [CardDef]
stage2s = [Quests.theLegacyOfMordor, Quests.memoryOfTheDarkYears, Quests.shrineToSauron, Quests.theLordOfMiddleEarth]

spec :: Spec
spec = describe "The Temple of Doom" do
  it "is a scenario the game can be dealt" . templeTest $ \_ ->
    isJust (lookupScenario "the-temple-of-doom") `shouldBe` True

  -- \* Setup

  it "stages Thane Ulchor and Tribute to Mordor, creates the Power of Mordor deck and puts the stage 2 cards under stage 1" . templeTest $ \pid -> do
    flipFirstQuest pid
    enemyCodes `shouldReturn` [toCardCode Enemies.thaneUlchorTheTempleOfDoom]
    _ <- tribute
    attrs <- scenarioAttrs
    sort (map toCardCode attrs.powerOfMordorDeck) `shouldBe` ["21066", "21067", "21068", "21069", "21070"]
    sort (map toCardCode attrs.questDeck) `shouldBe` sort (map toCardCode stage2s)
    -- The Temple of Doom's 8 others, Ulchor's Guard 9, Twisted Tunnels 9, Easterling Raiders 11
    length attrs.encounterDeck `shouldBe` 37
    any ((== Just ThePowerOfMordor) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` False
    copiesOf Enemies.thaneUlchorTheTempleOfDoom attrs.encounterDeck + copiesOf Quests.tributeToMordor attrs.encounterDeck `shouldBe` 0
    q <- mainQuest
    toCardCode q `shouldBe` toCardCode Quests.theForgottenTemple
    (toAttrs q).flipped `shouldBe` True

  it "builds the encounter deck with easy mode's fewer copies" . gameTestIn (overAttrs (modeL .~ EasyMode) theTempleOfDoom) testDeck $ \pid -> do
    flipFirstQuest pid
    deck <- (.encounterDeck) <$> scenarioAttrs
    length deck `shouldBe` 25
    copiesOf Locations.templeOfSauron deck `shouldBe` 1
    copiesOf Locations.hallOfHorrors deck `shouldBe` 2
    copiesOf Treacheries.placeOfEvil deck `shouldBe` 1
    copiesOf Enemies.ulchorsGuard deck `shouldBe` 3
    length . (.powerOfMordorDeck) <$> scenarioAttrs `shouldReturn` 5

  -- \* The Forgotten Temple

  it "skips the quest phase, and after two planning phases goes to the victory display and reveals the topmost stage 2A" . templeTest $ \pid -> do
    flipFirstQuest pid
    getModifiers GameTarget >>= (`shouldSatisfy` elem SkipQuestPhase)
    top : _ <- (.questDeck) <$> scenarioAttrs
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    toCardCode <$> mainQuest `shouldReturn` toCardCode Quests.theForgottenTemple
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    victoryCodes `shouldReturn` [toCardCode Quests.theForgottenTemple]
    toCardCode <$> mainQuest `shouldReturn` toCardCode top
    -- When Revealed: Reveal 1 encounter card per player.
    resolveRevealed
    locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
    (.flipped) . toAttrs <$> mainQuest `shouldReturn` True
    getModifiers GameTarget >>= (`shouldSatisfy` notElem SkipQuestPhase)

  -- \* Stage 2

  it "counts quest stages in the victory display, not side quests, for X" . templeTest $ \_ -> do
    thane <- stage Enemies.thaneUlchorTheTempleOfDoom
    getEnemyThreat thane `shouldReturn` 1
    inVictoryDisplay [Quests.theForgottenTemple, Quests.tributeToMordor, Quests.unwelcomeTravelers]
    getEnemyThreat thane `shouldReturn` 2

  it "takes no progress on a stage 2B with 4 quest stages in the victory display" . templeTest $ \_ -> do
    _ <- setQuestDeck [Quests.memoryOfTheDarkYears]
    q <- stageB Quests.theLegacyOfMordor
    stagesInVictory 4
    run $ PlaceTokens GameSource q Progress 3
    (.progress) . toAttrs <$> mainQuest `shouldReturn` 0

  it "adds a defeated stage 2 to the victory display and reveals the next" . templeTest $ \_ -> do
    _ <- setQuestDeck [Quests.memoryOfTheDarkYears]
    q <- stageB Quests.theLegacyOfMordor
    run $ PlaceTokens GameSource q Progress 8
    victoryCodes `shouldReturn` [toCardCode Quests.theLegacyOfMordor]
    toCardCode <$> mainQuest `shouldReturn` toCardCode Quests.memoryOfTheDarkYears

  it "has each player damage and exhaust a hero on The Legacy of Mordor, which cannot ready or be healed until the refresh phase" . templeTest $ \pid -> do
    _ <- stageB Quests.theLegacyOfMordor
    run $ CheckWindows [mkWhen $ PhaseBegins QuestPhase]
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    mods <- getModifiers beravor
    mods `shouldSatisfy` elem CannotReady
    mods `shouldSatisfy` elem CannotBeHealed

  it "adds 2 threat to the staging area for each card type of the X discarded on Memory of the Dark Years" . templeTest $ \_ -> do
    _ <- stageB Quests.memoryOfTheDarkYears
    stagesInVictory 1
    _ <- setEncounterDeck [Locations.oldForestRoad, Enemies.forestSpider, Locations.oldForestRoad]
    run $ CheckWindows [mkWhen $ PhaseBegins QuestPhase]
    length <$> getEncounterDeck `shouldReturn` 1
    getModifiers GameTarget >>= (`shouldSatisfy` elem (StagingThreatBonus 4))

  it "has each player discard the top X of his deck on Shrine to Sauron, and the copies he controls and holds" . templeTest $ \pid -> do
    _ <- stageB Quests.shrineToSauron
    run . PutOnTopOfDeck pid . PlayerCard =<< genPlayerCard Allies.guardOfTheCitadel (Just pid)
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    kept <- putCardIntoHand pid Allies.snowbournScout
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ CheckWindows [mkWhen $ PhaseBegins QuestPhase]
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [kept.id]
    length . filter ((== toCardCode Allies.guardOfTheCitadel) . toCardCode) . (.discard) <$> getPlayer pid `shouldReturn` 3

  it "gets 4 quest points per player on The Lord of Middle-earth, which takes no more progress this round than the cost discarded" . templeTest $ \pid -> do
    _ <- setQuestDeck [Quests.memoryOfTheDarkYears]
    q <- stageB Quests.theLordOfMiddleEarth
    card <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ CheckWindows [mkWhen $ PhaseBegins QuestPhase]
    chooseTarget pid (CardIdTarget card.id)
    (.hand) <$> getPlayer pid `shouldReturn` []
    run $ PlaceTokens GameSource q Progress 4
    (.progress) . toAttrs <$> mainQuest `shouldReturn` 2

  it "defeats The Lord of Middle-earth with 4 progress for one player" . templeTest $ \_ -> do
    _ <- setQuestDeck [Quests.memoryOfTheDarkYears]
    q <- stageB Quests.theLordOfMiddleEarth
    run $ PlaceTokens GameSource q Progress 3
    victoryCodes `shouldReturn` []
    run $ PlaceTokens GameSource q Progress 1
    victoryCodes `shouldReturn` [toCardCode Quests.theLordOfMiddleEarth]

  -- \* Tribute to Mordor

  it "places a damage token on Tribute to Mordor at the end of each round, and at 4 removes them to reveal the top card of the Power of Mordor deck" . templeTest $ \_ -> do
    t <- stageTribute
    setPowerOfMordorDeck [Locations.theTowerOfBaradDur]
    run $ CheckWindows [mkAfter RoundEnds]
    getDamage t `shouldReturn` 1
    run $ PlaceTokens GameSource t Damage 2
    run $ CheckWindows [mkAfter RoundEnds]
    getDamage t `shouldReturn` 0
    locationCodes `shouldReturn` [toCardCode Locations.theTowerOfBaradDur]

  it "loses the game when Tribute to Mordor cannot reveal a Power of Mordor card" . templeTest $ \_ -> do
    t <- stageTribute
    setPowerOfMordorDeck []
    run $ PlaceTokens GameSource t Damage 3
    run $ CheckWindows [mkAfter RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Lost

  it "keeps Tribute to Mordor from being the current quest while Thane Ulchor has hit points, and wins the game as it is defeated" . templeTest $ \_ -> do
    _ <- putQuestStage Quests.theLegacyOfMordor
    thane <- stage Enemies.thaneUlchorTheTempleOfDoom
    t <- stageTribute
    getModifiers t >>= (`shouldSatisfy` elem CannotBeCurrentQuest)
    withDamage thane 9
    getModifiers t >>= (`shouldSatisfy` notElem CannotBeCurrentQuest)
    run $ PlaceTokens GameSource t Progress 12
    getsGame (.outcome) `shouldReturn` Just Won
    victoryCodes >>= (`shouldSatisfy` elem (toCardCode Quests.tributeToMordor))

  -- \* Thane Ulchor

  it "has Thane Ulchor's threat be X, and lets him be engaged only when X is 5" . templeTest $ \pid -> do
    thane <- stage Enemies.thaneUlchorTheTempleOfDoom
    stagesInVictory 3
    getEnemyThreat thane `shouldReturn` 4
    mods <- getModifiers (EnemyTarget thane)
    mods `shouldSatisfy` elem (CannotEngagePlayer pid)
    mods `shouldSatisfy` elem ImmuneToPlayerCardEffects
    stagesInVictory 4
    getEnemyThreat thane `shouldReturn` 5
    getModifiers (EnemyTarget thane) >>= (`shouldSatisfy` notElem (CannotEngagePlayer pid))

  it "keeps Thane Ulchor in play past his hit points, as he is Indestructible" . templeTest $ \_ -> do
    thane <- stage Enemies.thaneUlchorTheTempleOfDoom
    withDamage thane 8
    run $ DealDamage GameSource (EnemyTarget thane) 3
    enemyCodes `shouldReturn` [toCardCode Enemies.thaneUlchorTheTempleOfDoom]

  it "places a damage token on Tribute to Mordor when Thane Ulchor destroys a character" . templeTest $ \pid -> do
    t <- stageTribute
    stagesInVictory 4
    thane <- stage Enemies.thaneUlchorTheTempleOfDoom
    run $ EngageEnemy pid thane
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    attackedBy pid thane [] guard'
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    getDamage t `shouldReturn` 1

  -- \* Encounter cards

  it "places a damage token on Tribute to Mordor as Temple of Sauron leaves play, and gives +1 attack for each quest stage in the victory display with its shadow" . templeTest $ \pid -> do
    t <- stageTribute
    temple <- stageLocation Locations.templeOfSauron
    run $ Discard GameSource (LocationTarget temple)
    getDamage t `shouldReturn` 1
    stagesInVictory 2
    spider <- stage Enemies.forestSpider
    run $ EngageEnemy pid spider
    beravor <- hero "01012"
    -- 2 attack, +2, against Beravor's 1 defense
    attackedBy pid spider [Locations.templeOfSauron] beravor
    getDamage beravor `shouldReturn` 3

  it "has Hall of Horrors' threat be X, and each player deal X damage to a character as it leaves play" . templeTest $ \pid -> do
    stagesInVictory 1
    hall <- stageLocation Locations.hallOfHorrors
    getLocationThreat hall `shouldReturn` 2
    run $ Discard GameSource (LocationTarget hall)
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2

  it "places 2 damage tokens on Tribute to Mordor with Place of Evil, or 1 and surges" . templeTest $ \pid -> do
    t <- stageTribute
    reveal [Treacheries.placeOfEvil, Locations.oldForestRoad]
    getDamage t `shouldReturn` 1
    chooseLabelStarting "$label.placeOfEvil.damage" pid
    getDamage t `shouldReturn` 2
    locationCodes `shouldReturn` []
    reveal [Treacheries.placeOfEvil, Locations.oldForestRoad]
    chooseLabelStarting "$label.placeOfEvil.surge" pid
    resolveRevealed
    getDamage t `shouldReturn` 3
    locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]

  it "gives +1 attack with Place of Evil's shadow, and places a damage token on Tribute to Mordor if the attack destroys a character" . templeTest $ \pid -> do
    t <- stageTribute
    spider <- stage Enemies.forestSpider
    run $ EngageEnemy pid spider
    -- Beravor's 1 defense against the spider's 2 attack, +1, with 2 damage on her 4 hit points
    beravor <- hero "01012"
    run $ PlaceTokens GameSource beravor Damage 2
    attackedBy pid spider [Treacheries.placeOfEvil] beravor
    getsGame (any ((== "01012") . toCardCode) . (.entities.heroes)) `shouldReturn` False
    getDamage t `shouldReturn` 1
