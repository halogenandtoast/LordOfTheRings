module LordOfTheRings.TheLordOfTheRings.AShadowOfThePastNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost)
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Scenario.Types (victoryDisplayL)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Helpers.Character (getWillpower, hideTestTotals)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Window (WindowType (..), mkAfter)
import LordOfTheRings.Scenario (aShadowOfThePast)
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn (overAttrs (modeL .~ NightmareMode) aShadowOfThePast) testDeck

tableNightmare :: ([PlayerId] -> GameT ()) -> IO ()
tableNightmare body = do
  seats <- traverse (\i -> either (error . show) id <$> loadPlayer ("player" <> tshow i) testDeck) (1 :| [2 :: Int])
  env <- newGameEnv (newTableIn (overAttrs (modeL .~ NightmareMode) aShadowOfThePast) seats) (const $ pure ()) 1
  runGameT env $ getsGame (.playerOrder) >>= body

passTableActions :: GameT ()
passTableActions = do
  questions <- getsGame (mapToList . (.question))
  case [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.takeAction"] of
    pid : _ -> passActions pid >> passTableActions
    [] -> pure ()

withRing :: PlayerId -> GameT Target
withRing pid = do
  run $ DoStep 3 Setup
  getQuestion pid >>= \case
    Just _ -> void $ answerQuestion pid 0
    Nothing -> pure ()
  ring <- fromJustNote "Ring" <$> getSagaOneRing
  pure $ toTarget ring

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  beforeIds <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (toId . fromJustNote "enemy" . find ((`notElem` beforeIds) . toId) . toList . (.entities.enemies))

spec :: Spec
spec = describe "A Shadow of the Past Nightmare" do
  it "replaces the prescribed standard encounter cards with all eight Nightmare card types" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.khamulTheEasterlingNightmare, Enemies.darkHorseman, Locations.eastfarthing, Locations.greenHillCountryNightmare, Locations.stockWoods, Treacheries.inTheNameOfMordor, Treacheries.followedByShadows, Treacheries.sniffingAndHissing]
      `shouldBe` [1, 2, 3, 3, 3, 2, 3, 2]
    map copies [Enemies.evilCrowAShadowOfThePast, Locations.woodyEnd, Locations.bamfurlong, Locations.stockBrook, Locations.greenHillCountry, Locations.pathlessCountryHunted, Treacheries.rodeLikeAGale]
      `shouldBe` replicate 7 0

  it "reduces each committed character separately without reducing ordinary willpower" . nightmare $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    theodred <- toTarget <$> getHeroWithCode "01002"
    horseman <- stageEnemy Enemies.darkHorseman
    run $ EngageEnemy pid horseman
    getWillpower eowyn `shouldReturn` 4
    hideTestTotals (HideTest pid GameSource 0 [eowyn, theodred] []) `shouldReturn` (3, 0)

  it "floors each character at zero when several Dark Horsemen are engaged" . nightmare $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    theodred <- toTarget <$> getHeroWithCode "01002"
    spearman <- toTarget <$> putAllyIntoPlay pid Allies.gondorianSpearman
    for_ [1 :: Int, 2] $ \_ -> do
      horseman <- stageEnemy Enemies.darkHorseman
      run $ EngageEnemy pid horseman
    hideTestTotals (HideTest pid GameSource 0 [eowyn, theodred, spearman] []) `shouldReturn` (2, 0)

  it "does not penalize hide tests while the Dark Horseman is in the staging area" . nightmare $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    _ <- stageEnemy Enemies.darkHorseman
    hideTestTotals (HideTest pid GameSource 0 [eowyn] []) `shouldReturn` (4, 0)

  it "stops offering characters after two have committed while Stock Woods is staged" . nightmare $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.stockWoods
    eowyn <- toTarget <$> getHeroWithCode "01007"
    theodred <- toTarget <$> getHeroWithCode "01002"
    run $ MakeHideTest (HideTest pid GameSource 0 [] [])
    chooseTarget pid eowyn
    chooseTarget pid theodred
    question <- getQuestion pid
    fmap (length . questionChoices) question `shouldBe` Just 1
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid

  it "adds a hide card for each staged Green-Hill Country and removes the modifier when active" . nightmare $ \_ -> do
    run . AddToStaging =<< genEncounterCard Locations.greenHillCountryNightmare
    lid <- getsGame (toId . fromJustNote "location" . find ((== toCardCode Locations.greenHillCountryNightmare) . toCardCode) . toList . (.entities.locations))
    getModifiers GameTarget >>= (`shouldSatisfy` elem (HideValue 1))
    run $ PlaceLocation lid ActiveLocation
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (HideValue 1))

  it "raises threat for a failed Followed by Shadows test" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Treacheries.followedByShadows, Locations.stockRoad, Locations.stockRoad]
    beforeThreat <- (.threat) <$> getPlayer pid
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 5)

  it "does not raise threat for a successful Followed by Shadows test" . nightmare $ \pid -> do
    _ <- setEncounterDeck [Treacheries.followedByShadows, Locations.stockRoad, Treacheries.lureOfTheRing]
    beforeThreat <- (.threat) <$> getPlayer pid
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid eowyn
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    (.threat) <$> getPlayer pid `shouldReturn` beforeThreat

  it "brings Khamul out of the encounter deck when stage three is revealed" . nightmare $ \_ -> do
    _ <- setEncounterDeck [Enemies.khamulTheEasterlingNightmare, Locations.stockRoad]
    _ <- putQuestStage Quests.escapeToBuckland
    q <- getActiveQuest
    run $ Flip (toTarget q)
    enemies <- getsGame (filter ((== toCardCode Enemies.khamulTheEasterlingNightmare) . toCardCode) . toList . (.entities.enemies))
    map (.placement) enemies `shouldBe` [StagingArea]
    deck <- getEncounterDeck
    any ((== toCardCode Enemies.khamulTheEasterlingNightmare) . toCardCode) deck `shouldBe` False

  it "does not move an already engaged Khamul when stage three is revealed" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.khamulTheEasterlingNightmare
    run $ EngageEnemy pid eid
    _ <- setEncounterDeck []
    _ <- putQuestStage Quests.escapeToBuckland
    q <- getActiveQuest
    run $ Flip (toTarget q)
    enemies <- getsGame (filter ((== toCardCode Enemies.khamulTheEasterlingNightmare) . toCardCode) . toList . (.entities.enemies))
    map (.placement) enemies `shouldBe` [EngagedWith pid]

  it "places a resource on each Eastfarthing after a failed hide test" . nightmare $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.eastfarthing
    lid <- getsGame (toId . fromJustNote "location" . find ((== toCardCode Locations.eastfarthing) . toCardCode) . toList . (.entities.locations))
    run $ CheckWindows [mkAfter $ FailedHideTest pid]
    getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem (ThreatModifier 2))
    run $ CheckWindows [mkAfter $ FailedHideTest pid]
    getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem (ThreatModifier 4))

  it "requires Hide 3 when Sniffing and Hissing is revealed without a ready Ring" . nightmare $ \pid -> do
    beforeThreat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.sniffingAndHissing, Treacheries.lureOfTheRing, Treacheries.lureOfTheRing, Treacheries.lureOfTheRing]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 2)
    length <$> getEncounterDiscard `shouldReturn` 4

  it "retrieves Khamul from the discard pile at stage three" . nightmare $ \_ -> do
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.khamulTheEasterlingNightmare
    _ <- putQuestStage Quests.escapeToBuckland
    q <- getActiveQuest
    run $ Flip (toTarget q)
    getsGame (length . filter ((== toCardCode Enemies.khamulTheEasterlingNightmare) . toCardCode) . toList . (.entities.enemies)) `shouldReturn` 1
    getEncounterDiscard `shouldReturn` []

  it "retrieves Khamul from the victory display at stage three" . nightmare $ \_ -> do
    card <- genEncounterCard Enemies.khamulTheEasterlingNightmare
    putGame . (scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard card])) =<< getGame
    _ <- putQuestStage Quests.escapeToBuckland
    q <- getActiveQuest
    run $ Flip (toTarget q)
    getsGame (length . filter ((== toCardCode Enemies.khamulTheEasterlingNightmare) . toCardCode) . toList . (.entities.enemies)) `shouldReturn` 1
    getsGame ((.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` []

  it "changes Khamul's engagement cost only while the Ring is exhausted" . nightmare $ \pid -> do
    ring <- withRing pid
    eid <- stageEnemy Enemies.khamulTheEasterlingNightmare
    getEngagementCost eid `shouldReturn` 40
    run $ Exhaust ring
    getEngagementCost eid `shouldReturn` 10
    run $ Ready ring
    getEngagementCost eid `shouldReturn` 40

  it "has Khamul make a refresh hide test before characters ready" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.khamulTheEasterlingNightmare
    run $ EngageEnemy pid eid
    exhausted <- toTarget <$> getHeroWithCode "01007"
    run $ Exhaust exhausted
    _ <- setEncounterDeck [Treacheries.lureOfTheRing, Treacheries.lureOfTheRing]
    run $ BeginPhase RefreshPhase
    question <- getQuestion pid
    fmap (length . questionChoices) question `shouldBe` Just 3
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` notElem AttackedThisPhase)

  it "has Khamul attack with a shadow after failing its own refresh hide test" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.khamulTheEasterlingNightmare
    run $ EngageEnemy pid eid
    _ <- setEncounterDeck [Locations.stockRoad, Locations.stockRoad, Treacheries.lureOfTheRing]
    run $ CheckWindows [mkAfter $ PhaseBegins RefreshPhase]
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem AttackedThisPhase)
    getsGame (map (length . (.shadowCards)) . toList . (.entities.enemies)) `shouldReturn` [1]

  it "penalizes only the player engaged with Dark Horseman" . tableNightmare $ \pids -> do
    let p1 = fromJustNote "first player" (headMay pids)
        p2 = fromJustNote "second player" (headMay $ drop 1 pids)
    horseman <- stageEnemy Enemies.darkHorseman
    run $ EngageEnemy p1 horseman
    getModifiers (PlayerTarget p1) >>= (`shouldSatisfy` elem (HideTestWillpower (-1)))
    getModifiers (PlayerTarget p2) >>= (`shouldSatisfy` notElem (HideTestWillpower (-1)))

  it "resolves Followed by Shadows in first-player order and raises only the failing player's threat" . tableNightmare $ \pids -> do
    let p1 = fromJustNote "first player" (headMay pids)
        p2 = fromJustNote "second player" (headMay $ drop 1 pids)
    eowyn <- getsGame (toTarget . fromJustNote "first player Eowyn" . find (\h -> h.owner == p1 && toCardCode h == "01007") . toList . (.entities.heroes))
    before1 <- (.threat) <$> getPlayer p1
    before2 <- (.threat) <$> getPlayer p2
    _ <- setEncounterDeck [Treacheries.followedByShadows, Locations.stockRoad, Treacheries.lureOfTheRing, Locations.stockRoad, Locations.stockRoad]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget p1 eowyn
    chooseLabel "$label.doneCommittingToHide" p1
    passTableActions
    chooseLabel "$label.doneCommittingToHide" p2
    passTableActions
    (.threat) <$> getPlayer p1 `shouldReturn` before1
    (.threat) <$> getPlayer p2 `shouldReturn` (before2 + 5)

  it "does not count an earlier failed hide test against a later successful Followed by Shadows test" . nightmare $ \pid -> do
    run $ FailHideTest pid
    beforeThreat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.followedByShadows, Treacheries.lureOfTheRing, Treacheries.lureOfTheRing]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    (.threat) <$> getPlayer pid `shouldReturn` beforeThreat

  it "waits for the remaining shadow's attack bonus before assigning undefended damage" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01007"
    receiver <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate receiver
    run $ FailHideTest pid
    followed <- genEncounterCard Treacheries.followedByShadows
    run $ AttachShadowCard eid followed
    run . AttachShadowCard eid =<< genEncounterCard Locations.stockWoods
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    chooseTarget pid (CardIdTarget followed.id)
    passActions pid
    chooseTarget pid receiver
    passActions pid
    getDamage defender `shouldReturn` 0
    getDamage receiver `shouldReturn` 5
    getModifiers defender >>= (`shouldSatisfy` notElem Defending)

  it "leaves a defended attack defended when no hide test failed this round" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.followedByShadows
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 0

  it "uses Stock Woods' larger shadow bonus only after a failed hide test" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    base <- getEnemyAttack eid
    run . AttachShadowCard eid =<< genEncounterCard Locations.stockWoods
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` (base + 2)

  it "uses Stock Woods' larger shadow bonus after a failed hide test" . nightmare $ \pid -> do
    run $ FailHideTest pid
    eid <- stageEnemy Enemies.goblinSniper
    base <- getEnemyAttack eid
    run . AttachShadowCard eid =<< genEncounterCard Locations.stockWoods
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` (base + 3)

  it "allows Sniffing and Hissing to exhaust the Ring and reveal another encounter card" . nightmare $ \pid -> do
    ring <- withRing pid
    _ <- setEncounterDeck [Treacheries.sniffingAndHissing, Locations.eastfarthing]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.sniffingAndHissing.exhaust" pid
    resolveRevealed
    getsGame (length . filter ((== toCardCode Locations.eastfarthing) . toCardCode) . toList . (.entities.locations)) `shouldReturn` 1
    -- A subsequent copy has no exhaustion option and goes straight to its hide test.
    _ <- setEncounterDeck [Treacheries.sniffingAndHissing, Treacheries.lureOfTheRing, Treacheries.lureOfTheRing, Treacheries.lureOfTheRing]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    run $ Ready ring

  it "gains Condition and discards a random card from the player who failed a hide test" . tableNightmare $ \pids -> do
    let p1 = fromJustNote "first" (headMay pids)
        p2 = fromJustNote "second" (headMay $ drop 1 pids)
    _ <- putCardIntoHand p1 Allies.guardOfTheCitadel
    _ <- putCardIntoHand p2 Allies.guardOfTheCitadel
    _ <- putQuestStage Quests.threeIsCompany
    quest <- toTarget <$> getActiveQuest
    _ <- setEncounterDeck [Treacheries.inTheNameOfMordor, Locations.stockRoad]
    run RevealEncounterCard
    resolveRevealed
    treachery <- getsGame (fromJustNote "condition" . find ((== toCardCode Treacheries.inTheNameOfMordor) . toCardCode) . toList . (.entities.treacheries))
    treachery.placement `shouldBe` AttachedTo quest
    getModifiers (toTarget treachery) >>= (`shouldSatisfy` elem (GainTrait Condition))
    run $ CheckWindows [mkAfter $ FailedHideTest p2]
    length . (.hand) <$> getPlayer p1 `shouldReturn` 1
    length . (.hand) <$> getPlayer p2 `shouldReturn` 0

  it "does not assign damage twice when Followed by Shadows resolves on an already undefended attack" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    run $ FailHideTest pid
    receiver <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.followedByShadows
    run $ EnemyAttack pid eid
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid receiver
    passActions pid
    getDamage receiver `shouldReturn` 2
    getQuestion pid `shouldReturn` Nothing

  it "adds Sniffing and Hissing's threat only if its attack destroys a character" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    beforeThreat <- (.threat) <$> getPlayer pid
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.sniffingAndHissing
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 3)

  it "does not add Sniffing and Hissing's threat when the defender survives" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    beforeThreat <- (.threat) <$> getPlayer pid
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.sniffingAndHissing
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 2
    (.threat) <$> getPlayer pid `shouldReturn` beforeThreat

  it "keeps negative hide modifiers inside the per-character zero floor" . nightmare $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ CreateEffect $ Effect GameSource eowyn [WillpowerModifier (-5)] UntilEndOfRound
    run $ CreateEffect $ Effect GameSource GameTarget [HideTestWillpower 1] UntilEndOfRound
    hideTestTotals (HideTest pid GameSource 0 [eowyn] []) `shouldReturn` (0, 0)

  it "protects a character whose willpower cannot be reduced during hide tests" . nightmare $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ CreateEffect $ Effect GameSource eowyn [CannotHaveWillpowerReduced] UntilEndOfRound
    run $ CreateEffect $ Effect GameSource GameTarget [HideTestWillpower 1] UntilEndOfRound
    horseman <- stageEnemy Enemies.darkHorseman
    run $ EngageEnemy pid horseman
    hideTestTotals (HideTest pid GameSource 0 [eowyn] []) `shouldReturn` (5, 0)

  it "passes Green-Hill Country's attacking enemy to the next player and deals a new shadow" . tableNightmare $ \pids -> do
    let p1 = fromJustNote "first" (headMay pids)
        p2 = fromJustNote "second" (headMay $ drop 1 pids)
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy p1 eid
    defender <- getsGame (toTarget . fromJustNote "first player defender" . find (\h -> h.owner == p1 && toCardCode h == "01012") . toList . (.entities.heroes))
    run . AttachShadowCard eid =<< genEncounterCard Locations.greenHillCountryNightmare
    _ <- setEncounterDeck [Treacheries.lureOfTheRing]
    run $ EnemyAttack p1 eid
    passTableActions
    chooseTarget p1 defender
    passTableActions
    enemies <- getsGame (toList . (.entities.enemies))
    map (.placement) enemies `shouldBe` [EngagedWith p2]
    map (length . (.shadowCards)) enemies `shouldBe` [2]
    isJust <$> getQuestion p2 `shouldReturn` True

  it "does not repeat Green-Hill Country's attack in a solo game" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Locations.greenHillCountryNightmare
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getQuestion pid `shouldReturn` Nothing

  it "includes Green-Hill Country's own staging modifier in its travel hide test" . nightmare $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.greenHillCountryNightmare
    lid <- getsGame (toId . fromJustNote "location" . find ((== toCardCode Locations.greenHillCountryNightmare) . toCardCode) . toList . (.entities.locations))
    _ <- setEncounterDeck [Treacheries.lureOfTheRing, Treacheries.lureOfTheRing]
    run $ TravelTo lid
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    length <$> getEncounterDiscard `shouldReturn` 2
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "makes Stock Woods' travel hide test before it leaves staging" . nightmare $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.stockWoods
    lid <- getsGame (toId . fromJustNote "location" . find ((== toCardCode Locations.stockWoods) . toCardCode) . toList . (.entities.locations))
    _ <- setEncounterDeck [Treacheries.lureOfTheRing]
    run $ TravelTo lid
    chooseLabel "$label.doneCommittingToHide" pid
    passActions pid
    length <$> getEncounterDiscard `shouldReturn` 1
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (HideCommitLimit 2))
