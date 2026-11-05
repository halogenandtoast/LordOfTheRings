module LordOfTheRings.OnTheDoorstep.TheLonelyMountainNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (PlayerCard (..))
import LordOfTheRings.Effect (EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationUnderneath))
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theLonelyMountain)
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (enemyProgress)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkWhen)
import TestImport hiding (before)

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) theLonelyMountain

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "perform quest setup" pid (const True)

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "no enemy"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.enemies)
    )

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "no location"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.locations)
    )

setHandAndDeck :: PlayerId -> [CardDef] -> [CardDef] -> GameT ()
setHandAndDeck pid hand deck = do
  h <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) hand
  d <- traverse (\c -> PlayerCard <$> genPlayerCard c (Just pid)) deck
  putGame . (playersL . ix pid . handL .~ h) . (playersL . ix pid . deckL .~ d) =<< getGame

attempt :: PlayerId -> CardDef -> [CardDef] -> CardDef -> GameT ()
attempt pid effect hand revealed = do
  setHandAndDeck pid hand [revealed]
  _ <- setEncounterDeck [effect]
  run $ ScenarioSpecific "burgle.attempt" (toJSON (ScenarioTarget, [] :: [Message], [] :: [Message]))

questCodes :: GameT [CardCode]
questCodes = getsGame (map toCardCode . toList . (.entities.quests))

passTableActions :: GameT ()
passTableActions = do
  questions <- getsGame (mapToList . (.question))
  case [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.takeAction"] of
    pid : _ -> passActions pid >> passTableActions
    [] -> pure ()

spec :: Spec
spec = describe "The Lonely Mountain Nightmare" do
  it "builds the nightmare deck with the exact removals and all six replacement card types"
    . nightmare
    $ \_ -> do
      run $ DoStep 1 Setup
      cards <- getEncounterDeck
      length cards `shouldBe` 40
      let copies def = length $ filter ((== toCardCode def) . toCardCode) cards
      map
        copies
        [ Enemies.darkBats
        , Treacheries.fightingAmongFriends
        , Locations.westernSlopes
        , Locations.secretEntrance
        , Treacheries.belchingFire
        ]
        `shouldBe` [0, 0, 0, 0, 0]
      map
        copies
        [ Enemies.ereborBats
        , Locations.secretTunnel
        , Locations.southernSpur
        , Locations.theFrontGateNightmare
        , Treacheries.theDragonsIre
        , Treacheries.lustForTreasure
        ]
        `shouldBe` [4, 3, 3, 2, 4, 3]
      getsGame (length . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 4

  it "keeps the standard deck unchanged" . gameTestIn theLonelyMountain testDeck $ \_ -> do
    run $ DoStep 1 Setup
    length <$> getEncounterDeck `shouldReturn` 35

  it "blocks advancing to stage 3 while treasure remains under the mountain" . nightmare $ \pid -> do
    setUp pid
    stages <- getsGame (map toId . toList . (.entities.quests))
    for_ stages $ run . LeaveQuestStage
    run AdvanceQuest
    questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]
    run AdvanceQuest
    questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]
    card <- genEncounterCard Quests.insideInformation
    run $ AdvanceToQuest card
    questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]

  it "allows advancing once every stacked card has been taken" . nightmare $ \pid -> do
    setUp pid
    stages <- getsGame (map toId . toList . (.entities.quests))
    for_ stages $ run . LeaveQuestStage
    run AdvanceQuest
    mountain <- getsGame (fromJustNote "mountain" . headMay . toList . (.entities.locations))
    let (firstFour, finalCard) = splitAt 4 (locationUnderneath $ toAttrs mountain)
        takeTreasure = \case
          PlayerCard pc -> run $ PutInHand pc {pcOwner = Just pid}
          _ -> pure ()
    traverse_ takeTreasure firstFour
    run AdvanceQuest
    questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]
    traverse_ takeTreasure finalCard
    quest <- getsGame (fromJustNote "quest" . headMay . toList . (.entities.quests))
    run $ DoStep 1 (UseCardAbility pid (toSource quest) 1 [])
    chooseLabel "$label.theBurglarsTurnTheLonelyMountain.advanceToStage3" pid
    resolveRevealed
    questCodes `shouldReturn` [toCardCode Quests.insideInformation]

  it "protects Smaug from all damage until A Bare Patch is attached to him" . nightmare $ \_ -> do
    smaug <- stageEnemy Enemies.smaugTheMagnificent
    run $ DealDamage GameSource (EnemyTarget smaug) 3
    getDamage (EnemyTarget smaug) `shouldReturn` 0
    run . AddToStaging =<< genEncounterCard Objectives.aBarePatch
    patch <- getsGame (toId . fromJustNote "patch" . headMay . toList . (.entities.objectives))
    run $ ClaimObjective patch (EnemyTarget smaug)
    run $ DealDamage GameSource (EnemyTarget smaug) 3
    getDamage (EnemyTarget smaug) `shouldReturn` 3
    run $ PlaceObjective patch StagingArea
    run $ DealDamage GameSource (EnemyTarget smaug) 2
    getDamage (EnemyTarget smaug) `shouldReturn` 3

  it "has Erebor Bats discard from the defending player's hand when it attacks"
    . twoPlayerTestIn nightmareScenario testDeck
    $ \one two -> do
      e <- stageEnemy Enemies.ereborBats
      _ <- putCardIntoHand one Events.sneakAttack
      _ <- putCardIntoHand two Events.sneakAttack
      oneBefore <- length . (.hand) <$> getPlayer one
      twoBefore <- length . (.hand) <$> getPlayer two
      run $ CheckWindows [mkWhen $ EnemyAttacking e two]
      length . (.hand) <$> getPlayer one `shouldReturn` oneBefore
      length . (.hand) <$> getPlayer two `shouldReturn` (twoBefore - 1)

  it "matches Erebor Bats by cost and type, requiring two cards regardless of sphere" . nightmare $ \pid -> do
    attempt
      pid
      Enemies.ereborBats
      [Allies.guardOfTheCitadel, Allies.defenderOfRammas]
      Allies.guardOfTheCitadel
    chooseLabel "$label.theLonelyMountain.burgle.discard" pid
    void $ answerQuestion pid 0
    length . (.hand) <$> getPlayer pid `shouldReturn` 0

  it "does not allow paying just one matching card for Erebor Bats" . nightmare $ \pid -> do
    attempt pid Enemies.ereborBats [Allies.guardOfTheCitadel] Allies.guardOfTheCitadel
    question <- getQuestion pid
    question
      `shouldSatisfy` maybe
        False
        ( not
            . any (\case Label "$label.theLonelyMountain.burgle.discard" _ -> True; _ -> False)
            . questionChoices
        )

  it "requires three cards matching only cost for The Front Gate's burgle" . nightmare $ \pid -> do
    attempt
      pid
      Locations.theFrontGateNightmare
      [Allies.snowbournScout, Events.feint, Attachments.dunedainMark]
      Events.sneakAttack
    chooseLabel "$label.theLonelyMountain.burgle.discard" pid
    void $ answerQuestion pid 0
    void $ answerQuestion pid 0
    length . (.hand) <$> getPlayer pid `shouldReturn` 0

  it "matches all of sphere, type and cost for Lust for Treasure's burgle" . nightmare $ \pid -> do
    attempt pid Treacheries.lustForTreasure [Events.sneakAttack] Events.sneakAttack
    question <- getQuestion pid
    question
      `shouldSatisfy` maybe False (\case QuestionLabel label _ _ _ -> "sphereTypeAndCost" `isInfixOf` label; _ -> False)
    chooseLabel "$label.theLonelyMountain.burgle.discard" pid
    length . (.hand) <$> getPlayer pid `shouldReturn` 0

  it "rejects a card matching only two of Lust for Treasure's three requirements" . nightmare $ \pid -> do
    attempt
      pid
      Treacheries.lustForTreasure
      [Events.feint, Allies.snowbournScout, Events.grimResolve]
      Events.sneakAttack
    question <- getQuestion pid
    question
      `shouldSatisfy` maybe
        False
        ( not
            . any (\case Label "$label.theLonelyMountain.burgle.discard" _ -> True; _ -> False)
            . questionChoices
        )

  it "has every Secret Tunnel raise each player's threat after a failed burgle"
    . twoPlayerTestIn nightmareScenario testDeck
    $ \one two -> do
      _ <- stageLocation Locations.secretTunnel
      _ <- stageLocation Locations.secretTunnel
      oneBefore <- (.threat) <$> getPlayer one
      twoBefore <- (.threat) <$> getPlayer two
      run $ ScenarioSpecific "burgle.result" (toJSON (ScenarioTarget, False))
      (.threat) <$> getPlayer one `shouldReturn` (oneBefore + 4)
      (.threat) <$> getPlayer two `shouldReturn` (twoBefore + 4)

  it "does not penalize a successful burgle at Secret Tunnel" . nightmare $ \pid -> do
    _ <- stageLocation Locations.secretTunnel
    before <- (.threat) <$> getPlayer pid
    run $ ScenarioSpecific "burgle.result" (toJSON (ScenarioTarget, True))
    (.threat) <$> getPlayer pid `shouldReturn` before

  it "requires Bilbo to be ready with a resource to travel to Secret Tunnel" . nightmare $ \pid -> do
    setUp pid
    l <- stageLocation Locations.secretTunnel
    getModifiers (LocationTarget l) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    bilbo <- getHeroWithCode "132001"
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 1
    getModifiers (LocationTarget l) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo l
    getHeroResources bilbo.id `shouldReturn` 0
    (.heroExhausted) . toAttrs <$> getHeroWithCode "132001" `shouldReturn` True

  it "has Southern Spur notice an ally played before the location entered play" . nightmare $ \pid -> do
    card <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PlayFromHandAtNoCost pid card.id
    _ <- stageLocation Locations.southernSpur
    before <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    (.threat) <$> getPlayer pid `shouldReturn` (before + 1)

  it "does not count an ally merely put into play for Southern Spur" . nightmare $ \pid -> do
    _ <- stageLocation Locations.southernSpur
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    before <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    (.threat) <$> getPlayer pid `shouldReturn` before

  it "clears Southern Spur's played-ally record at the end of the phase" . nightmare $ \pid -> do
    card <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PlayFromHandAtNoCost pid card.id
    run $ EndEffects UntilEndOfPhase
    _ <- stageLocation Locations.southernSpur
    before <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    (.threat) <$> getPlayer pid `shouldReturn` before

  it "gives Southern Spur's shadow +2 attack" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    before <- getEnemyAttack e
    run . AttachShadowCard e =<< genEncounterCard Locations.southernSpur
    run $ ResolveShadow e pid []
    getEnemyAttack e `shouldReturn` (before + 2)

  it "has Lust for Treasure count attachments on heroes and allies, plus Doomed 1" . nightmare $ \pid -> do
    h <- toTarget <$> getHeroWithCode "01012"
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- putAttachmentOn pid Attachments.citadelPlate h
    _ <- putAttachmentOn pid Attachments.dunedainMark ally
    before <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.lustForTreasure]
    run RevealEncounterCard
    resolveRevealed
    (.threat) <$> getPlayer pid `shouldReturn` (before + 3)

  it "does not double count Condition attachments for Lust for Treasure" . nightmare $ \pid -> do
    h <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.unexpectedCourage h
    e <- stageEnemy Enemies.darkBats
    run $ PlaceEnemy e (AttachedTo h)
    before <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.lustForTreasure]
    run RevealEncounterCard
    resolveRevealed
    (.threat) <$> getPlayer pid `shouldReturn` (before + 3)

  it "has The Front Gate give each Dragon a shadow card before attacking the first player" . nightmare $ \pid -> do
    dragon <- stageEnemy Enemies.smaugTheGolden
    _ <- setEncounterDeck [Enemies.goblinSniper]
    l <- stageLocation Locations.theFrontGateNightmare
    run $ PlaceTokens GameSource (LocationTarget l) Progress 2
    length . (.shadowCards) <$> getEnemy dragon `shouldReturn` 1
    defender <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate defender
    passTableActions
    chooseTarget pid defender
    passTableActions
    getDamage defender `shouldReturn` 4

  it
    "has The Dragon's Ire count casualties across all its attacks, then put that much progress on every Dragon"
    . nightmare
    $ \pid -> do
      golden <- stageEnemy Enemies.smaugTheGolden
      magnificent <- stageEnemy Enemies.smaugTheMagnificent
      firstDefender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      secondDefender <- putAllyIntoPlay pid Allies.snowbournScout
      before <- (.threat) <$> getPlayer pid
      _ <- setEncounterDeck [Treacheries.theDragonsIre, Enemies.goblinSniper]
      run RevealEncounterCard
      resolveRevealed
      passTableActions
      chooseTarget pid firstDefender
      passTableActions
      enemyProgress golden `shouldReturn` 0
      enemyProgress magnificent `shouldReturn` 0
      chooseTarget pid secondDefender
      passTableActions
      enemyProgress golden `shouldReturn` 2
      enemyProgress magnificent `shouldReturn` 2
      length . (.shadowCards) <$> getEnemy golden `shouldReturn` 0
      length . (.shadowCards) <$> getEnemy magnificent `shouldReturn` 0
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.goblinSniper]
      (.threat) <$> getPlayer pid `shouldReturn` (before + 1)

  it "places no progress for The Dragon's Ire if its attacks destroy no characters" . nightmare $ \pid -> do
    dragon <- stageEnemy Enemies.smaugTheGolden
    defender <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate defender
    _ <- setEncounterDeck [Treacheries.theDragonsIre]
    run RevealEncounterCard
    resolveRevealed
    passTableActions
    chooseTarget pid defender
    passTableActions
    getDamage defender `shouldReturn` 4
    enemyProgress dragon `shouldReturn` 0

  it "has Southern Spur's shadow raise every player's threat if the attack destroys a character" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    e <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy one e
    defender <- putAllyIntoPlay one Allies.guardOfTheCitadel
    run . AttachShadowCard e =<< genEncounterCard Locations.southernSpur
    oneBefore <- (.threat) <$> getPlayer one
    twoBefore <- (.threat) <$> getPlayer two
    run $ EnemyAttack one e
    passTableActions
    chooseTarget one defender
    passTableActions
    (.threat) <$> getPlayer one `shouldReturn` (oneBefore + 3)
    (.threat) <$> getPlayer two `shouldReturn` (twoBefore + 3)

  it "does not trigger Southern Spur's shadow threat penalty without a casualty" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid e
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard e =<< genEncounterCard Locations.southernSpur
    before <- (.threat) <$> getPlayer pid
    run $ EnemyAttack pid e
    passTableActions
    chooseTarget pid defender
    passTableActions
    getDamage defender `shouldReturn` 2
    (.threat) <$> getPlayer pid `shouldReturn` before

  it "has The Dragon's Ire attack each player in turn order without dealing shadow cards" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    dragon <- stageEnemy Enemies.smaugTheGolden
    firstDefender <- putAllyIntoPlay one Allies.guardOfTheCitadel
    secondDefender <- putAllyIntoPlay two Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Treacheries.theDragonsIre, Enemies.goblinSniper]
    run RevealEncounterCard
    resolveRevealed
    passTableActions
    chooseTarget one firstDefender
    passTableActions
    enemyProgress dragon `shouldReturn` 0
    chooseTarget two secondDefender
    passTableActions
    enemyProgress dragon `shouldReturn` 2
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.goblinSniper]

  it "does not offer the optional advance at stage 2 while any treasure remains" . nightmare $ \pid -> do
    setUp pid
    stages <- getsGame (map toId . toList . (.entities.quests))
    for_ stages $ run . LeaveQuestStage
    run AdvanceQuest
    quest <- getsGame (fromJustNote "quest" . headMay . toList . (.entities.quests))
    run $ DoStep 1 (UseCardAbility pid (toSource quest) 1 [])
    getQuestion pid `shouldReturn` Nothing
    questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]
