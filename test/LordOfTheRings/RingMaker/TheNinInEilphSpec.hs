module LordOfTheRings.RingMaker.TheNinInEilphSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (AnyCard), getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theNinInEilph)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

ninTest :: (PlayerId -> GameT ()) -> IO ()
ninTest = gameTestIn theNinInEilph testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

questDeckCodes :: GameT [CardCode]
questDeckCodes = map toCardCode . (.questDeck) <$> scenarioAttrs

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

-- | Put the stage into play on its B side, its When Revealed resolved.
revealStage :: PlayerId -> CardDef -> GameT Target
revealStage pid def = do
  q <- putQuestStage def
  run $ Flip q
  resolveWhenRevealed pid
  toTarget <$> getActiveQuest

activeStage :: GameT CardCode
activeStage = toCardCode <$> getActiveQuest

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [a.placement | a <- toList g.entities.allies, toCardCode a == code]

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

stageTwos :: [CardDef]
stageTwos = [Quests.throughTheMarshNoEndInSight, Quests.throughTheMarshAWearyPassage, Quests.throughTheMarshAForgottenLand]

stageThrees :: [CardDef]
stageThrees =
  [ Quests.lostInTheSwanfleetImpassableMarshland
  , Quests.lostInTheSwanfleetATreacherousSwamp
  , Quests.lostInTheSwanfleetCreaturesOfAForgottenAge
  ]

spec :: Spec
spec = describe "The Nîn-in-Eilph" do
  it "is a scenario the game can be dealt" . ninTest $ \_ ->
    isJust (lookupScenario "the-nin-in-eilph") `shouldBe` True

  it "gives Nalir to the first player, sets the Marsh-dweller aside, stages a location and advances to a stage 2" . ninTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    _ <- setEncounterDeck [Objectives.nalirTheNinInEilph, Enemies.ancientMarshDweller, Locations.hiddenEyot, Enemies.neekerbreekers]
    chooseOptionMatching "perform the setup" pid (const True)
    resolveWhenRevealed pid
    placementOf Objectives.nalirTheNinInEilph `shouldReturn` Just (InPlayArea pid)
    placementOf Locations.hiddenEyot `shouldReturn` Just StagingArea
    map toCardCode . scenarioSetAside <$> scenarioAttrs `shouldReturn` [toCardCode Enemies.ancientMarshDweller]
    stage <- activeStage
    map toCardCode stageTwos `shouldSatisfy` elem stage
    deck <- questDeckCodes
    length deck `shouldBe` 6
    deck `shouldSatisfy` notElem stage
    threatOf pid `shouldReturn` 28
    getTimeCounters . toTarget =<< getActiveQuest
    `shouldReturn` 3

  it "wanders to a different stage 2 as its last time counter goes, returning itself to the quest deck" . ninTest $ \pid -> do
    setQuestDeck [Quests.throughTheMarshAWearyPassage, Quests.lostInTheSwanfleetImpassableMarshland]
    q <- revealStage pid Quests.throughTheMarshNoEndInSight
    run $ RemoveTokens GameSource q TimeCounter 3
    resolveWhenRevealed pid
    activeStage `shouldReturn` toCardCode Quests.throughTheMarshAWearyPassage
    questDeckCodes >>= (`shouldSatisfy` elem (toCardCode Quests.throughTheMarshNoEndInSight))
    getTimeCounters . toTarget =<< getActiveQuest
    `shouldReturn` 3

  it "advances to a random stage 3 once a stage 2 is defeated, leaving the other stage 2s behind" . ninTest $ \pid -> do
    setQuestDeck $ [Quests.throughTheMarshAWearyPassage] <> stageThrees <> [Quests.outOfTheSwamp]
    q <- revealStage pid Quests.throughTheMarshNoEndInSight
    run $ PlaceTokens GameSource q Progress 13
    resolveWhenRevealed pid
    stage <- activeStage
    map toCardCode stageThrees `shouldSatisfy` elem stage
    deck <- questDeckCodes
    deck `shouldSatisfy` notElem (toCardCode Quests.throughTheMarshAWearyPassage)
    length deck `shouldBe` 3

  it "brings Ancient Marsh-dweller out of play as a stage 3 is revealed" . ninTest $ \pid -> do
    card <- genEncounterCard Enemies.ancientMarshDweller
    run $ SetAside card
    _ <- revealStage pid Quests.lostInTheSwanfleetImpassableMarshland
    placementOf Enemies.ancientMarshDweller `shouldReturn` Just StagingArea
    map toCardCode . scenarioSetAside <$> scenarioAttrs `shouldReturn` []

  it "returns an engaged, damaged Ancient Marsh-dweller to the staging area healed" . ninTest $ \pid -> do
    eid <- staged Enemies.ancientMarshDweller
    run $ EngageEnemy pid eid
    run $ DealDamage GameSource (toTarget eid) 3
    _ <- revealStage pid Quests.lostInTheSwanfleetATreacherousSwamp
    placementOf Enemies.ancientMarshDweller `shouldReturn` Just StagingArea
    getDamage (toTarget eid) `shouldReturn` 0

  it "puts a resource on Ancient Marsh-dweller as time counters leave the quest, for +1 threat and attack each" . ninTest $ \pid -> do
    q <- revealStage pid Quests.throughTheMarshNoEndInSight
    eid <- staged Enemies.ancientMarshDweller
    run $ RemoveTokens GameSource q TimeCounter 1
    getEnemyThreat eid `shouldReturn` 4
    getEnemyAttack eid `shouldReturn` 7

  it "lets a player ready no more than 5 characters in the refresh phase under A Treacherous Swamp" . ninTest $ \pid -> do
    _ <- revealStage pid Quests.lostInTheSwanfleetATreacherousSwamp
    allies <- traverse (putAllyIntoPlay pid) [Allies.guardOfTheCitadel, Allies.snowbournScout, Allies.gondorianSpearman]
    heroes <- map toTarget . toList <$> getsGame (.entities.heroes)
    for_ (heroes <> allies) (run . Exhaust)
    run $ BeginPhase RefreshPhase
    for_ (take 5 $ heroes <> allies) (chooseTarget pid)
    exhausted <- getsGame \g -> length [() | a <- toList g.entities.allies, allyExhausted (toAttrs a)]
    exhausted `shouldBe` 1

  it "discards a card at random from a player's hand as he commits under A Weary Passage" . ninTest $ \pid -> do
    _ <- revealStage pid Quests.throughTheMarshAWearyPassage
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    hand <- length . (.hand) <$> getPlayer pid
    run $ BeginPhase QuestPhase
    chooseTarget pid =<< getHeroWithCode "01007"
    length . (.hand) <$> getPlayer pid `shouldReturn` hand - 1
    chooseTarget pid =<< getHeroWithCode "01012"
    length . (.hand) <$> getPlayer pid `shouldReturn` hand - 1

  it "bars player card effects from drawing or gaining resources under No End in Sight" . ninTest $ \pid -> do
    _ <- revealStage pid Quests.throughTheMarshNoEndInSight
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotDrawCardsFromCardEffects)
    eowyn <- getHeroWithCode "01007"
    run $ PlaceTokens (HeroSource eowyn.id) (toTarget eowyn) Resource 1
    getHeroResources eowyn.id `shouldReturn` 0
    -- an encounter card's effect still may
    run $ PlaceTokens ScenarioSource (toTarget eowyn) Resource 1
    getHeroResources eowyn.id `shouldReturn` 1

  it "lets each player play only 1 card each round under A Forgotten Land" . ninTest $ \pid -> do
    _ <- revealStage pid Quests.throughTheMarshAForgottenLand
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))
    _ <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (CannotPlay AnyCard))

  it "counts a card played earlier in the round under A Forgotten Land" . ninTest $ \pid -> do
    _ <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    passActions pid
    _ <- revealStage pid Quests.throughTheMarshAForgottenLand
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (CannotPlay AnyCard))

  it "gives enemies in the staging area -20 engagement cost under Creatures of a Forgotten Age" . ninTest $ \pid -> do
    _ <- revealStage pid Quests.lostInTheSwanfleetCreaturesOfAForgottenAge
    adder <- staged Enemies.giantSwampAdder
    getModifiers (toTarget adder) >>= (`shouldSatisfy` elem (EngagementCostModifier (-20)))

  it "has Ancient Marsh-dweller attack each player as Out of the Swamp is revealed" . ninTest $ \pid -> do
    card <- genEncounterCard Enemies.ancientMarshDweller
    run $ SetAside card
    _ <- revealStage pid Quests.outOfTheSwamp
    placementOf Enemies.ancientMarshDweller `shouldReturn` Just StagingArea
    passActions pid
    question <- getQuestion pid
    question `shouldSatisfy` any (any (\case Label "$label.takeAttackUndefended" _ -> True; _ -> False) . questionChoices)

  it "is won as Ancient Marsh-dweller is defeated during Out of the Swamp" . ninTest $ \_ -> do
    _ <- putQuestStage Quests.outOfTheSwamp
    eid <- staged Enemies.ancientMarshDweller
    outcome `shouldReturn` Nothing
    run $ Destroy GameSource (toTarget eid)
    outcome `shouldReturn` Just Won

  it "raises Nalir's controller's threat as the refresh phase begins, and loses the game as he leaves play" . ninTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.nalirTheNinInEilph
    threat <- threatOf pid
    run $ BeginPhase RefreshPhase
    -- 1 for the refresh phase, and 1 for Nalir with 1 player in the game
    threatOf pid `shouldReturn` threat + 2
    passActions pid
    nalir <- getsGame (fromJustNote "no Nalir" . fmap toTarget . headMay . toList . (.entities.allies))
    run $ Discard nalir
    outcome `shouldReturn` Just Lost

  it "has each player exhaust a character as the players advance while Fen of Reeds is in the staging area" . ninTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.fenOfReeds
    setQuestDeck [Quests.throughTheMarshAWearyPassage]
    run AdvanceQuest
    resolveWhenRevealed pid
    eowyn <- getHeroWithCode "01007"
    chooseTarget pid eowyn
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True

  it "places 2 time counters on the quest as Hidden Eyot is explored" . ninTest $ \pid -> do
    q <- revealStage pid Quests.throughTheMarshNoEndInSight
    run . AddToStaging =<< genEncounterCard Locations.hiddenEyot
    eyot <- getsGame (fromJustNote "no Eyot" . fmap toTarget . headMay . toList . (.entities.locations))
    run $ PlaceTokens GameSource eyot Progress 4
    chooseAbilityOf pid eyot
    getTimeCounters q `shouldReturn` 5

  it "removes a time counter from the quest as Shifting Marshland is revealed" . ninTest $ \pid -> do
    q <- revealStage pid Quests.throughTheMarshNoEndInSight
    _ <- setEncounterDeck [Treacheries.shiftingMarshland, Locations.hiddenEyot]
    run RevealEncounterCard
    resolveRevealed
    getTimeCounters q `shouldReturn` 2

  it "has an engaged Giant Swamp Adder attack as time counters leave the quest" . ninTest $ \pid -> do
    q <- revealStage pid Quests.throughTheMarshNoEndInSight
    adder <- staged Enemies.giantSwampAdder
    run $ EngageEnemy pid adder
    run $ RemoveTokens GameSource q TimeCounter 1
    passActions pid
    question <- getQuestion pid
    question `shouldSatisfy` any (any (\case Label "$label.takeAttackUndefended" _ -> True; _ -> False) . questionChoices)

  it "has each player put a Creature enemy into play engaged with him as Remnants of Elder Days is revealed" . ninTest $ \pid -> do
    cards <- setEncounterDeck [Treacheries.remnantsOfElderDays, Enemies.neekerbreekers]
    run RevealEncounterCard
    resolveRevealed
    for_ (drop 1 cards) (chooseTarget pid)
    placementOf Enemies.neekerbreekers `shouldReturn` Just (EngagedWith pid)

  it "gives a character -1 to its stats for each Item attached while Sinking Bog is in the staging area" . ninTest $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    _ <- putAttachmentOn pid Attachments.bladeOfGondolin eowyn
    _ <- putAttachmentOn pid Attachments.citadelPlate eowyn
    run . AddToStaging =<< genEncounterCard Locations.sinkingBog
    run . AddToStaging =<< genEncounterCard Locations.sinkingBog
    mods <- getModifiers eowyn
    [n | WillpowerModifier n <- mods] `shouldBe` [-2]
    [n | DefenseModifier n <- mods] `shouldBe` [-2]

  it "has the player engaged with Neekerbreekers deal 2 damage to an ally as time counters leave the quest" . ninTest $ \pid -> do
    q <- revealStage pid Quests.throughTheMarshNoEndInSight
    faramir <- putAllyIntoPlay pid Allies.faramir
    neeker <- staged Enemies.neekerbreekers
    run $ EngageEnemy pid neeker
    run $ RemoveTokens GameSource q TimeCounter 1
    getDamage faramir `shouldReturn` 2

  it "removes 1 progress from each location in play at the end of the round while Finger of Glanduin is in the staging area" . ninTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Locations.fingerOfGlanduin
    run . AddToStaging =<< genEncounterCard Locations.sinkingBog
    locations <- getsGame (map toTarget . toList . (.entities.locations))
    for_ locations \l -> run $ PlaceTokens GameSource l Progress 2
    run EndRound
    progress <- getsGame (map (.progress) . toList . (.entities.locations))
    progress `shouldBe` [1, 1]
