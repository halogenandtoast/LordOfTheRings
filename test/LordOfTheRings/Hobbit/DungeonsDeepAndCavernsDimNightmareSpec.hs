module LordOfTheRings.Hobbit.DungeonsDeepAndCavernsDimNightmareSpec (spec) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Attachment.Types (attachmentExhausted)
import LordOfTheRings.Effect (EffectDuration (UntilEndOfAttack, UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (attachedWithTrait, getEnemy, getEnemyDefense)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (dungeonsDeepAndCavernsDim)
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (Named (..))
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (..), modeL, questDeckL)
import LordOfTheRings.Trait (Trait (Condition, Scout))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport hiding (before, first, second)

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) dungeonsDeepAndCavernsDim

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

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

withBilbo :: GameT Hero
withBilbo = run (DoStep 3 Setup) >> getHeroWithCode "131001"

ring :: GameT ObjectiveAttrs
ring =
  getsGame
    ( toAttrs
        . fromJustNote "no ring"
        . find ((== toCardCode Objectives.bilbosMagicRing) . toCardCode)
        . toList
        . (.entities.objectives)
    )

atRiddles :: GameT ()
atRiddles = do
  _ <- withBilbo
  _ <-
    setEncounterDeck
      [ Locations.lakeInTheCavern
      , Enemies.gollumDungeonsDeep
      , Objectives.bilbosMagicRing
      , Locations.theWargsGlade
      ]
  later <- traverse genEncounterCard [Quests.riddlesInTheDark, Quests.intoTheFire]
  putGame . (scenarioL %~ overAttrs (questDeckL .~ later)) =<< getGame
  card <- genEncounterCard Quests.outOfTheFryingPan
  run $ PutQuestIntoPlay card
  run $ PerformSetup (CardIdTarget card.id)

setDeck :: PlayerId -> [CardDef] -> GameT ()
setDeck pid defs = do
  cards <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) defs
  putGame . (playersL . ix pid . deckL .~ cards) =<< getGame

riddleProgress :: GameT Int
riddleProgress =
  getsGame
    ( maybe 0 (.progress)
        . find ((== toCardCode Quests.riddlesInTheDark) . toCardCode)
        . toList
        . (.entities.quests)
    )

chooseNamed :: PlayerId -> Named -> GameT ()
chooseNamed pid named = chooseOptionMatching "name the riddle item" pid \case
  Label _ msgs ->
    any
      (\case ScenarioSpecific "riddle.name" value -> parseMaybe parseJSON value == Just named; _ -> False)
      msgs
  _ -> False

revealNormally :: CardDef -> [CardDef] -> GameT ()
revealNormally def rest = do
  _ <- setEncounterDeck (def : rest)
  run RevealEncounterCard
  lead <- getLead
  getQuestion lead >>= \case
    Just (QuestionLabel prompt _ _ _) | "$label.riddle.choose" `isPrefixOf` prompt -> chooseLabel "$label.riddle.resolve" lead
    _ -> pure ()
  resolveRevealed

spec :: Spec
spec = describe "Dungeons Deep and Caverns Dim Nightmare" do
  it "removes the specified copies, gathers the Nightmare cards, and sets The Lower Gate aside" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    attrs <- getsGame (toAttrs . (.scenario))
    map
      (`copies` attrs.encounterDeck)
      [ Enemies.goblinMiners
      , Enemies.goblinAxeman
      , Locations.theGoblinsCaves
      , Treacheries.gripGrabPinchNab
      , Treacheries.notFairNotFair
      ]
      `shouldBe` [0, 0, 0, 0, 0]
    map
      (`copies` attrs.encounterDeck)
      [ Enemies.wickedWolf
      , Enemies.lowerGateSentries
      , Locations.pineSlopes
      , Locations.fallenStones
      , Treacheries.whereIsssIt
      , Treacheries.weHatesIt
      ]
      `shouldBe` replicate 6 3
    map toCardCode attrs.scenarioSetAside `shouldBe` [toCardCode Locations.theLowerGate]
    copies Locations.theLowerGate attrs.encounterDeck `shouldBe` 0

  it "keeps the normal deck unchanged" . gameTestIn dungeonsDeepAndCavernsDim testDeck $ \_ -> do
    run $ DoStep 1 Setup
    cards <- getEncounterDeck
    copies Enemies.goblinMiners cards `shouldBe` 3
    copies Locations.theLowerGate cards `shouldBe` 0
    copies Enemies.wickedWolf cards `shouldBe` 0

  it "prevents all damage to Gollum in Nightmare mode" . nightmare $ \_ -> do
    atRiddles
    gollum <-
      getsGame
        ( toId
            . fromJustNote "no Gollum"
            . find ((== toCardCode Enemies.gollumDungeonsDeep) . toCardCode)
            . toList
            . (.entities.enemies)
        )
    run $ DealDamage GameSource (EnemyTarget gollum) 3
    getDamage (EnemyTarget gollum) `shouldReturn` 0

  it "allows encounter damage to Gollum in normal mode" . gameTestIn dungeonsDeepAndCavernsDim testDeck $ \_ -> do
    atRiddles
    gollum <-
      getsGame
        ( toId
            . fromJustNote "no Gollum"
            . find ((== toCardCode Enemies.gollumDungeonsDeep) . toCardCode)
            . toList
            . (.entities.enemies)
        )
    run $ DealDamage GameSource (EnemyTarget gollum) 1
    getDamage (EnemyTarget gollum) `shouldReturn` 1

  it "stages the set-aside Lower Gate as stage 3 turns over" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    initial <- getActiveQuest
    run $ LeaveQuestStage initial.id
    _ <- setEncounterDeck [Locations.theHighPass]
    run . AdvanceToQuest =<< genEncounterCard Quests.intoTheFire
    resolveRevealed
    getsGame (sort . map toCardCode . toList . (.entities.locations))
      `shouldReturn` sort (map toCardCode [Locations.theLowerGate, Locations.theHighPass])
    getsGame (length . (.scenarioSetAside) . toAttrs . (.scenario)) `shouldReturn` 0

  it "caps progress on the current quest at 4 per round, then allows another 4 the next round" . nightmare $ \_ -> do
    q <- putQuestStage Quests.intoTheFire
    _ <- stageLocation Locations.theLowerGate
    run $ PlaceTokens GameSource q Progress 3
    run $ PlaceTokens GameSource q Progress 3
    (.progress) <$> getActiveQuest `shouldReturn` 4
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource q Progress 5
    (.progress) <$> getActiveQuest `shouldReturn` 8

  it "counts progress already placed this round when The Lower Gate enters play" . nightmare $ \_ -> do
    q <- putQuestStage Quests.intoTheFire
    run $ PlaceTokens GameSource q Progress 2
    _ <- stageLocation Locations.theLowerGate
    run $ PlaceTokens GameSource q Progress 4
    (.progress) <$> getActiveQuest `shouldReturn` 4

  it "gives Creatures +1 attack and Goblins +1 defense while The Lower Gate is in play" . nightmare $ \_ -> do
    wolf <- stageEnemy Enemies.wickedWolf
    sentries <- stageEnemy Enemies.lowerGateSentries
    _ <- stageLocation Locations.theLowerGate
    getEnemyAttack wolf `shouldReturn` 5
    getEnemyDefense sentries `shouldReturn` 5
    getEnemyDefense wolf `shouldReturn` 3
    getEnemyAttack sentries `shouldReturn` 3

  it "answers Fallen Stones by title, counting all matching discarded cards" . nightmare $ \pid -> do
    atRiddles
    setDeck pid (replicate 3 Allies.snowbournScout <> replicate 2 Allies.guardOfTheCitadel)
    _ <- setEncounterDeck [Locations.fallenStones]
    run RevealEncounterCard
    chooseLabel "$label.riddle.answer" pid
    chooseNamed pid (NamedTitle "Snowbourn Scout")
    passActions pid
    riddleProgress `shouldReturn` 3
    copies Locations.fallenStones <$> getEncounterDiscard `shouldReturn` 1
    getsGame (map toCardCode . toList . (.entities.locations))
      `shouldReturn` [toCardCode Locations.lakeInTheCavern]

  it "offers card titles outside the answering player's deck too" . nightmare $ \pid -> do
    atRiddles
    setDeck pid [Allies.snowbournScout]
    _ <- setEncounterDeck [Locations.fallenStones]
    run RevealEncounterCard
    chooseLabel "$label.riddle.answer" pid
    chooseNamed pid (NamedTitle "Gandalf")
    -- The new title is accepted, and the failed riddle leads to Gollum's attack.
    passActions pid
    getQuestion pid
      >>= ( `shouldSatisfy`
              \case Just (QuestionLabel prompt _ _ _) -> "$label.declareDefender" `isPrefixOf` prompt; _ -> False
          )

  it "answers We Hates It! by a trait, counting a card with that trait among several" . nightmare $ \pid -> do
    atRiddles
    setDeck pid [Allies.snowbournScout]
    _ <- setEncounterDeck [Treacheries.weHatesIt]
    run RevealEncounterCard
    chooseLabel "$label.riddle.answer" pid
    chooseNamed pid (NamedTrait Scout)
    passActions pid
    riddleProgress `shouldReturn` 1
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "requires a sphere, cost, and card type together for Where Isss It's riddle" . nightmare $ \pid -> do
    atRiddles
    setDeck pid [Events.sneakAttack, Events.sneakAttack, Events.grimResolve]
    _ <- setEncounterDeck [Treacheries.whereIsssIt]
    run RevealEncounterCard
    chooseLabel "$label.riddle.answer" pid
    chooseLabel "$label.riddle.leadership" pid
    void $ answerQuestion pid 1
    chooseLabel "$label.riddle.event" pid
    passActions pid
    riddleProgress `shouldReturn` 2

  it "exhausts a character immediately when Wicked Wolf receives a riddle shadow" . nightmare $ \pid -> do
    wolf <- stageEnemy Enemies.wickedWolf
    run $ EngageEnemy pid wolf
    hero <- getHeroWithCode "01007"
    run . AttachShadowCard wolf =<< genEncounterCard Locations.fallenStones
    chooseTarget pid hero
    (.heroExhausted) . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
    (.shadowCards)
      <$> getEnemy wolf
      >>= (`shouldSatisfy` all ((== toCardCode Locations.fallenStones) . toCardCode))

  it "does not exhaust a character for Wicked Wolf's non-riddle shadows" . nightmare $ \pid -> do
    wolf <- stageEnemy Enemies.wickedWolf
    run $ EngageEnemy pid wolf
    run . AttachShadowCard wolf =<< genEncounterCard Locations.pineSlopes
    getQuestion pid `shouldReturn` Nothing
    (.heroExhausted) . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False

  it "returns an attacking enemy to staging after Wicked Wolf's shadow resolves" . nightmare $ \pid -> do
    enemy <- stageEnemy Enemies.goblinMiners
    run $ EngageEnemy pid enemy
    run . AttachShadowCard enemy =<< genEncounterCard Enemies.wickedWolf
    run $ EnemyAttack pid enemy
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid =<< getHeroWithCode "01007"
    (.placement) <$> getEnemy enemy `shouldReturn` StagingArea

  it
    "keeps Bilbo's objective Ring exhausted during refresh, while allowing it to be readied by an action"
    . nightmare
    $ \pid -> do
      atRiddles
      _ <- stageEnemy Enemies.lowerGateSentries
      objective <- ring
      run $ Exhaust (toTarget objective)
      run $ BeginPhase RefreshPhase
      passActions pid
      (.exhausted) <$> ring `shouldReturn` True
      run $ Ready (toTarget objective)
      (.exhausted) <$> ring `shouldReturn` False

  it "also prevents the treasure printing of Bilbo's Ring from refreshing" . nightmare $ \pid -> do
    bilbo <- withBilbo
    treasure <- putAttachmentOn pid Attachments.bilbosMagicRing (toTarget bilbo)
    _ <- stageEnemy Enemies.lowerGateSentries
    run $ Exhaust treasure
    run $ BeginPhase RefreshPhase
    passActions pid
    getModifiers treasure >>= (`shouldSatisfy` elem DoesNotReadyDuringRefresh)
    getsGame
      ( attachmentExhausted
          . toAttrs
          . fromJustNote "no treasure"
          . find ((== treasure) . toTarget)
          . toList
          . (.entities.attachments)
      )
      `shouldReturn` True

  it "gives Lower Gate Sentries' shadow +1 attack, or +3 while Bilbo's Ring is exhausted" . nightmare $ \pid -> do
    atRiddles
    enemy <- stageEnemy Enemies.goblinMiners
    original <- getEnemyAttack enemy
    run . AttachShadowCard enemy =<< genEncounterCard Enemies.lowerGateSentries
    run $ ResolveShadow enemy pid []
    getEnemyAttack enemy `shouldReturn` (original + 1)
    run $ EndEffects UntilEndOfAttack
    run . Exhaust . toTarget =<< ring
    run . AttachShadowCard enemy =<< genEncounterCard Enemies.lowerGateSentries
    run $ ResolveShadow enemy pid []
    getEnemyAttack enemy `shouldReturn` (original + 3)

  it "has Pine Slopes trigger for each Baggins resource spent, raising every player's threat" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    bilbo <- withBilbo
    _ <- stageLocation Locations.pineSlopes
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 3
    beforeOne <- (.threat) <$> getPlayer one
    beforeTwo <- (.threat) <$> getPlayer two
    run $ SpendResourcesFrom one [(toTarget bilbo, 2)]
    (.threat) <$> getPlayer one `shouldReturn` (beforeOne + 2)
    (.threat) <$> getPlayer two `shouldReturn` (beforeTwo + 2)
    getHeroResources bilbo.id `shouldReturn` 1

  it "triggers Pine Slopes only for resources actually discarded, including an oversized discard" . nightmare $ \pid -> do
    bilbo <- withBilbo
    _ <- stageLocation Locations.pineSlopes
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 2
    before <- (.threat) <$> getPlayer pid
    run $ RemoveTokens GameSource (toTarget bilbo) Resource 5
    (.threat) <$> getPlayer pid `shouldReturn` (before + 2)
    run $ RemoveTokens GameSource (toTarget bilbo) Resource 1
    (.threat) <$> getPlayer pid `shouldReturn` (before + 2)

  it "does not trigger Pine Slopes or mark resources spent when Baggins resources are moved" . nightmare $ \pid -> do
    bilbo <- withBilbo
    receiver <- getHeroWithCode "01012"
    _ <- stageLocation Locations.pineSlopes
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 2
    before <- (.threat) <$> getPlayer pid
    run $ MoveResourcesFromHero GameSource bilbo.id (toTarget receiver) 2
    (.threat) <$> getPlayer pid `shouldReturn` before
    getHeroResources bilbo.id `shouldReturn` 0
    getHeroResources receiver.id `shouldReturn` 2
    getModifiers bilbo >>= (`shouldSatisfy` notElem SpentResourcesThisRound)

  it "does not trigger Pine Slopes for another sphere's resource" . nightmare $ \pid -> do
    _ <- stageLocation Locations.pineSlopes
    hero <- getHeroWithCode "01002"
    run $ PlaceTokens GameSource (toTarget hero) Resource 1
    before <- (.threat) <$> getPlayer pid
    run $ SpendResourcesFrom pid [(toTarget hero, 1)]
    (.threat) <$> getPlayer pid `shouldReturn` before

  it "deals and resolves 2 additional shadows during an attack for Pine Slopes" . nightmare $ \pid -> do
    enemy <- stageEnemy Enemies.goblinMiners
    original <- getEnemyAttack enemy
    _ <- setEncounterDeck [Enemies.goblinMiners, Enemies.goblinMiners]
    run . AttachShadowCard enemy =<< genEncounterCard Locations.pineSlopes
    run $ ResolveShadow enemy pid []
    resolveShadowsInOrder pid
    length . (.shadowCards) <$> getEnemy enemy `shouldReturn` 3
    getEnemyAttack enemy `shouldReturn` (original + 2)

  it "damages only ready characters when traveling to Fallen Stones without spending resources" . nightmare $ \_ -> do
    stones <- stageLocation Locations.fallenStones
    exhausted <- getHeroWithCode "01007"
    defender <- getHeroWithCode "01012"
    run $ Exhaust (toTarget exhausted)
    run $ TravelTo stones
    getDamage (toTarget defender) `shouldReturn` 3
    getDamage (toTarget exhausted) `shouldReturn` 0
    (.placement) <$> getLocation stones `shouldReturn` ActiveLocation

  it "reduces Fallen Stones' damage for every ready character by the resources paid" . nightmare $ \pid -> do
    bilbo <- withBilbo
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 2
    stones <- stageLocation Locations.fallenStones
    run $ TravelTo stones
    void $ answerQuestion pid 2
    getHeroResources bilbo.id `shouldReturn` 0
    getDamage (toTarget bilbo) `shouldReturn` 1
    hero <- getHeroWithCode "01012"
    getDamage (toTarget hero) `shouldReturn` 1

  it "caps Fallen Stones' optional payment at 3 and prevents all its damage when 3 is paid" . nightmare $ \pid -> do
    bilbo <- withBilbo
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 5
    stones <- stageLocation Locations.fallenStones
    run $ TravelTo stones
    Just question <- getQuestion pid
    length (questionChoices question) `shouldBe` 4
    void $ answerQuestion pid 3
    getHeroResources bilbo.id `shouldReturn` 2
    getDamage (toTarget bilbo) `shouldReturn` 0

  it "can discard 2 resources for Where Isss It instead of Gollum attacking" . nightmare $ \pid -> do
    atRiddles
    bilbo <- getHeroWithCode "131001"
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 2
    revealNormally Treacheries.whereIsssIt []
    chooseLabel "$label.whereIsssIt.discard" pid
    getHeroResources bilbo.id `shouldReturn` 0
    getDamage (toTarget bilbo) `shouldReturn` 0
    getQuestion pid `shouldReturn` Nothing

  it
    "makes Gollum attack with a shadow and assigns undefended damage to Bilbo when 2 resources cannot be discarded"
    . nightmare
    $ \pid -> do
      atRiddles
      bilbo <- getHeroWithCode "131001"
      run $ PlaceTokens GameSource (toTarget bilbo) Resource 1
      revealNormally Treacheries.whereIsssIt [Locations.theHighPass]
      passActions pid
      gollum <-
        getsGame
          ( toId
              . fromJustNote "no Gollum"
              . find ((== toCardCode Enemies.gollumDungeonsDeep) . toCardCode)
              . toList
              . (.entities.enemies)
          )
      length . (.shadowCards) <$> getEnemy gollum `shouldReturn` 1
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      getDamage (toTarget bilbo) `shouldReturn` 2
      getHeroResources bilbo.id `shouldReturn` 1

  it "attaches We Hates It to Gollum, granting +2 attack only on stage 3" . nightmare $ \_ -> do
    atRiddles
    gollum <-
      getsGame
        ( toId
            . fromJustNote "no Gollum"
            . find ((== toCardCode Enemies.gollumDungeonsDeep) . toCardCode)
            . toList
            . (.entities.enemies)
        )
    revealNormally Treacheries.weHatesIt []
    getEnemyAttack gollum `shouldReturn` 2
    attachedWithTrait Condition (EnemyTarget gollum) >>= (`shouldSatisfy` (not . null))
    getsGame (map (.placement) . toList . (.entities.treacheries))
      `shouldReturn` [AttachedTo $ EnemyTarget gollum]
    stages <- getsGame (map toId . toList . (.entities.quests))
    for_ stages $ run . LeaveQuestStage
    _ <- putQuestStage Quests.intoTheFire
    getEnemyAttack gollum `shouldReturn` 4
