module LordOfTheRings.TheLordOfTheRings.AKnifeInTheDarkNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getDefense)
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing, theOneRingExhausted)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (aKnifeInTheDark)
import LordOfTheRings.Scenario.Types (
  GameMode (NightmareMode),
  modeL,
  scenarioSetAside,
  setAsideL,
  victoryDisplayL,
 )
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn (overAttrs (modeL .~ NightmareMode) aKnifeInTheDark) testDeck

tableNightmare :: ([PlayerId] -> GameT ()) -> IO ()
tableNightmare body = do
  seats <-
    traverse
      (\i -> either (error . show) id <$> loadPlayer ("player" <> tshow i) testDeck)
      (1 :| [2 :: Int])
  env <-
    newGameEnv
      (newTableIn (overAttrs (modeL .~ NightmareMode) aKnifeInTheDark) seats)
      (const $ pure ())
      1
  runGameT env $ getsGame (.playerOrder) >>= body

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  beforeIds <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame
    (toId . fromJustNote "enemy" . find ((`notElem` beforeIds) . toId) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  beforeIds <- getsGame (map toId . toList . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "location"
        . find ((`notElem` beforeIds) . toId)
        . toList
        . (.entities.locations)
    )

withRing :: PlayerId -> GameT Target
withRing pid = do
  run $ DoStep 3 Setup
  getQuestion pid >>= \case
    Just _ -> void $ answerQuestion pid 0
    Nothing -> pure ()
  toTarget . fromJustNote "Ring" <$> getSagaOneRing

setAsideWraiths :: Int -> GameT ()
setAsideWraiths n = do
  cards <- replicateM n (genEncounterCard Enemies.ringwraith)
  putGame . (scenarioL %~ overAttrs (setAsideL .~ cards)) =<< getGame

roadResources :: LocationId -> GameT Int
roadResources lid = countTokens Resource . locationTokens . toAttrs <$> getLocation lid

spec :: Spec
spec = describe "A Knife in the Dark Nightmare" do
  it "replaces standard encounter cards and sets the Nightmare Weathertop aside" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map
      copies
      [ Enemies.shadyBreeLander
      , Locations.weatherHills
      , Locations.pathlessCountryHunted
      , Locations.weathertop
      , Treacheries.unwantedAttention
      ]
      `shouldBe` [0, 0, 0, 0, 2]
    map
      copies
      [ Enemies.menacingWraith
      , Enemies.southernSpy
      , Locations.weatherHillsNightmare
      , Locations.theOldRoadNightmare
      , Locations.forsakenInn
      , Treacheries.theRingsWill
      , Treacheries.bladeTip
      ]
      `shouldBe` [2, 4, 3, 3, 2, 2, 2]
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    length (filter ((== toCardCode Locations.weathertopNightmare) . toCardCode) aside) `shouldBe` 1
    length (filter ((== toCardCode Locations.weathertop) . toCardCode) aside) `shouldBe` 0

  it "makes Bill Ferny immune only while he is in staging" . nightmare $ \pid -> do
    bill <- stageEnemy Enemies.billFerny
    getModifiers (EnemyTarget bill) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    run $ EngageEnemy pid bill
    getModifiers (EnemyTarget bill) >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)

  it "keeps standard Bill Ferny vulnerable to player effects" . gameTestIn aKnifeInTheDark testDeck $ \_ -> do
    bill <- stageEnemy Enemies.billFerny
    getModifiers (EnemyTarget bill) >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)

  it "adds the Nightmare Weathertop at stage three" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    -- Isolate stage three from the earlier quest that setup put in play.
    old <- getActiveQuest
    run $ LeaveQuestStage (toId old)
    _ <- putQuestStage Quests.theRingwraithsAttack
    q <- getActiveQuest
    run $ Flip (toTarget q)
    locations <- getsGame (map toCardCode . toList . (.entities.locations))
    locations `shouldSatisfy` elem (toCardCode Locations.weathertopNightmare)
    locations `shouldSatisfy` notElem (toCardCode Locations.weathertop)

  it "has Menacing Wraith engage and attack the first player with a shadow when the Ring exhausts" . nightmare $ \pid -> do
    ring <- withRing pid
    eid <- stageEnemy Enemies.menacingWraith
    _ <- setEncounterDeck [Treacheries.unwantedAttention]
    run $ Exhaust ring
    passActions pid
    enemies <- getsGame (toList . (.entities.enemies))
    map (.placement) enemies `shouldBe` [EngagedWith pid]
    map (length . (.shadowCards)) enemies `shouldBe` [1]
    getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem AttackedThisPhase)

  it "allows Southern Spy to shuffle one set-aside Ringwraith instead of attacking" . nightmare $ \pid -> do
    setAsideWraiths 2
    _ <- setEncounterDeck [Enemies.southernSpy]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.aKnifeInTheDark.shuffleRingwraith" pid
    deck <- getEncounterDeck
    map toCardCode deck `shouldBe` [toCardCode Enemies.ringwraith]
    getsGame (length . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 1

  it "has Southern Spy attack the revealer when no Ringwraith remains set aside" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Enemies.southernSpy, Treacheries.unwantedAttention]
    run RevealEncounterCard
    resolveRevealed
    passActions pid
    enemies <- getsGame (toList . (.entities.enemies))
    map (length . (.shadowCards)) enemies `shouldBe` [1]
    for_ enemies $ \e -> getModifiers e >>= (`shouldSatisfy` elem AttackedThisPhase)

  it "reduces a defender's defense by 1 when the Ring is ready" . nightmare $ \pid -> do
    _ <- withRing pid
    eid <- stageEnemy Enemies.goblinSniper
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Enemies.southernSpy
    run $ ResolveShadow eid pid [defender]
    getDefense defender `shouldReturn` 1

  it "reduces a defender's defense by 3 when the Ring is exhausted" . nightmare $ \pid -> do
    ring <- withRing pid
    run $ Exhaust ring
    eid <- stageEnemy Enemies.goblinSniper
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Enemies.southernSpy
    run $ ResolveShadow eid pid [defender]
    getDefense defender `shouldReturn` 0
    getModifiers defender >>= (`shouldSatisfy` elem (DefenseModifier (-3)))

  it "prevents damage to the Witch-king while Weathertop is in play" . nightmare $ \_ -> do
    _ <- stageLocation Locations.weathertopNightmare
    eid <- stageEnemy Enemies.theWitchKing
    run $ DealDamage GameSource (EnemyTarget eid) 3
    getDamage (EnemyTarget eid) `shouldReturn` 0

  it "exhausts and locks the Ring when Weathertop becomes active by an effect" . nightmare $ \pid -> do
    ring <- withRing pid
    lid <- stageLocation Locations.weathertopNightmare
    run $ PlaceLocation lid ActiveLocation
    theOneRingExhausted `shouldReturn` True
    run $ Ready ring
    theOneRingExhausted `shouldReturn` True
    run $ PlaceLocation lid StagingArea
    run $ Ready ring
    theOneRingExhausted `shouldReturn` False

  it "blocks progress on Weather Hills at stage one and allows it at stage two" . nightmare $ \_ -> do
    _ <- putQuestStage Quests.troubleInBree
    lid <- stageLocation Locations.weatherHillsNightmare
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 2
    (.progress) <$> getLocation lid `shouldReturn` 0
    q <- getActiveQuest
    run $ LeaveQuestStage (toId q)
    _ <- putQuestStage Quests.intoTheWild
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 2
    (.progress) <$> getLocation lid `shouldReturn` 2

  it "raises Weather Hills' threat while the Ring is exhausted" . nightmare $ \pid -> do
    ring <- withRing pid
    lid <- stageLocation Locations.weatherHillsNightmare
    getLocationThreat lid `shouldReturn` 3
    run $ Exhaust ring
    getLocationThreat lid `shouldReturn` 5

  it "gives Weather Hills' shadow +2 attack" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run . AttachShadowCard eid =<< genEncounterCard Locations.weatherHillsNightmare
    run $ ResolveShadow eid pid []
    getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem (AttackModifier 2))

  it "starts The Old Road at three resources and adds a Ringwraith on every round after it empties" . nightmare $ \pid -> do
    setAsideWraiths 3
    lid <- stageLocation Locations.theOldRoadNightmare
    roadResources lid `shouldReturn` 3
    for_ [2, 1, 0, 0] $ \expected -> do
      run $ CheckWindows [mkAfter RoundEnds]
      roadResources lid `shouldReturn` expected
    deck <- getEncounterDeck
    length (filter ((== toCardCode Enemies.ringwraith) . toCardCode) deck) `shouldBe` 2
    getQuestion pid `shouldReturn` Nothing

  it "does not give standard The Old Road the Nightmare card's countdown" . nightmare $ \_ -> do
    lid <- stageLocation Locations.theOldRoad
    roadResources lid `shouldReturn` 0
    run $ CheckWindows [mkAfter RoundEnds]
    roadResources lid `shouldReturn` 0

  it "sets Forsaken Inn's threat from the first player's characters" . nightmare $ \pid -> do
    lid <- stageLocation Locations.forsakenInn
    getLocationThreat lid `shouldReturn` 3
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    getLocationThreat lid `shouldReturn` 4

  it "reveals a searched Spy as Forsaken Inn's travel effect" . nightmare $ \pid -> do
    lid <- stageLocation Locations.forsakenInn
    setAsideWraiths 1
    _ <- setEncounterDeck [Enemies.southernSpy]
    run $ TravelTo lid
    chooseOptionMatching "find Southern Spy" pid (const True)
    resolveRevealed
    chooseLabel "$label.aKnifeInTheDark.shuffleRingwraith" pid
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    getsGame
      (length . filter ((== toCardCode Enemies.southernSpy) . toCardCode) . toList . (.entities.enemies))
      `shouldReturn` 1

  it "raises threat by five for The Ring's Will" . nightmare $ \pid -> do
    beforeThreat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.theRingsWill]
    run RevealEncounterCard
    resolveRevealed
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 5)

  it "attaches Blade Tip to a surviving defender damaged by the attack" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01002"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.bladeTip
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 1
    treacheries <- getsGame (toList . (.entities.treacheries))
    map (.placement) treacheries `shouldBe` [AttachedTo defender]
    beforeThreat <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 2)

  it "does not attach Blade Tip if the defender takes no damage" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.bladeTip
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "does not attach Blade Tip to the hero taking an undefended attack" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.bladeTip
    run $ EnemyAttack pid eid
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 2
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "surges and raises threat for Blade Tip revealed as a treachery" . nightmare $ \pid -> do
    beforeThreat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.bladeTip, Locations.weatherHillsNightmare]
    run RevealEncounterCard
    resolveRevealed
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 1)
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "allows A Test of Will to cancel The Ring's Will while the Ring is ready" . nightmare $ \pid -> do
    _ <- withRing pid
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    card <- putCardIntoHand pid Events.aTestOfWill
    beforeThreat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.theRingsWill]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    (.threat) <$> getPlayer pid `shouldReturn` beforeThreat

  it "does not offer A Test of Will for The Ring's Will while the Ring is exhausted" . nightmare $ \pid -> do
    ring <- withRing pid
    run $ Exhaust ring
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    _ <- putCardIntoHand pid Events.aTestOfWill
    beforeThreat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.theRingsWill]
    run RevealEncounterCard
    resolveRevealed
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 5)

  it "blocks the Ring-bearer's cancellation while keeping ordinary encounter cancellation available" . nightmare $ \pid -> do
    _ <- withRing pid
    frodo <- toTarget <$> getHeroWithCode (toCardCode Heroes.frodoBagginsTheBlackRiders)
    run $ PlaceTokens GameSource frodo Resource 1
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    card <- putCardIntoHand pid Events.aTestOfWill
    _ <- setEncounterDeck [Treacheries.theRingsWill]
    run RevealEncounterCard
    offersAbilityOf pid frodo `shouldReturn` False
    getModifiers frodo >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)
    getModifiers GameTarget >>= (`shouldSatisfy` notElem CannotCancelEncounterEffects)
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getModifiers frodo >>= (`shouldSatisfy` notElem CannotCancelEncounterEffects)

  it "does not attach Blade Tip to a defender destroyed by the attack" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.bladeTip
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    getsGame (length . (.entities.allies)) `shouldReturn` 0

  it "has The Ring's Will raise every player's threat" . tableNightmare $ \pids -> do
    beforeThreats <- traverse (fmap (.threat) . getPlayer) pids
    _ <- setEncounterDeck [Treacheries.theRingsWill]
    run RevealEncounterCard
    resolveRevealed
    traverse (fmap (.threat) . getPlayer) pids `shouldReturn` map (+ 5) beforeThreats

  it "has Menacing Wraith leave another player to attack the first player" . tableNightmare $ \pids -> do
    let p1 = fromJustNote "first" (headMay pids)
        p2 = fromJustNote "second" (headMay $ drop 1 pids)
    ring <- withRing p1
    eid <- stageEnemy Enemies.menacingWraith
    run $ EngageEnemy p2 eid
    _ <- setEncounterDeck [Treacheries.unwantedAttention]
    run $ Exhaust ring
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [EngagedWith p1]

  it "does not count another player's characters for Forsaken Inn" . tableNightmare $ \pids -> do
    let p2 = fromJustNote "second" (headMay $ drop 1 pids)
    _ <- putAllyIntoPlay p2 Allies.guardOfTheCitadel
    lid <- stageLocation Locations.forsakenInn
    getLocationThreat lid `shouldReturn` 3

  it "lets Southern Spy shuffle a discarded Ringwraith when the set-aside pool is empty" . nightmare $ \pid -> do
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.ringwraith
    _ <- setEncounterDeck [Enemies.southernSpy]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.aKnifeInTheDark.shuffleRingwraith" pid
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.ringwraith]
    getEncounterDiscard `shouldReturn` []

  it "lets The Old Road recover an out-of-play Ringwraith from the victory display" . nightmare $ \_ -> do
    wraith <- genEncounterCard Enemies.ringwraith
    putGame . (scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard wraith])) =<< getGame
    _ <- stageLocation Locations.theOldRoadNightmare
    replicateM_ 3 $ run $ CheckWindows [mkAfter RoundEnds]
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.ringwraith]
    getsGame ((.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` []

  it "allows choosing the discard pile while leaving the set-aside Ringwraith available" . nightmare $ \pid -> do
    setAsideWraiths 1
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.ringwraith
    _ <- setEncounterDeck [Enemies.southernSpy]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.aKnifeInTheDark.shuffleRingwraith" pid
    chooseLabel "$label.aKnifeInTheDark.ringwraithFrom.discard" pid
    getEncounterDiscard `shouldReturn` []
    getsGame (length . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 1

  it
    "does not attach Blade Tip after a later shadow makes its original defender take undefended damage" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01002"
    run $ FailHideTest pid
    blade <- genEncounterCard Treacheries.bladeTip
    run $ AttachShadowCard eid blade
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.followedByShadows
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    chooseTarget pid (CardIdTarget blade.id)
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 2
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "does not let questing progress bypass an active Weather Hills at stage one" . nightmare $ \_ -> do
    _ <- putQuestStage Quests.troubleInBree
    lid <- stageLocation Locations.weatherHillsNightmare
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceProgressOnQuest 4
    (.progress) <$> getLocation lid `shouldReturn` 0
    (.progress) <$> getActiveQuest `shouldReturn` 0
