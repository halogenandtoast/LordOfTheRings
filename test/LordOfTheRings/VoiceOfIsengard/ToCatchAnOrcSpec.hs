module LordOfTheRings.VoiceOfIsengard.ToCatchAnOrcSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (CannotReady))
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, toCatchAnOrc)
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (mugashHost, outOfPlayDeckOf)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), metaL, questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (EnemyAttacking, PhaseBegins, QuestedSuccessfully), mkAfter, mkWhen)
import TestImport

orcTest :: (PlayerId -> GameT ()) -> IO ()
orcTest = gameTestIn toCatchAnOrc testDeck

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  stage def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

-- | The player's out-of-play deck is set to the cards, top first.
setOutOfPlayDeck :: PlayerId -> [Card] -> GameT ()
setOutOfPlayDeck pid cards = do
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (metaL .~ toJSON (Map.singleton pid cards))

encounter :: CardDef -> GameT Card
encounter def = EncounterCard <$> genEncounterCard def

playerCard :: PlayerId -> CardDef -> GameT Card
playerCard pid def = PlayerCard <$> genPlayerCard def (Just pid)

handCodes :: PlayerId -> GameT [CardCode]
handCodes pid = map toCardCode . (.hand) <$> getPlayer pid

ally :: CardDef
ally = Allies.snowbournScout

-- | Mugash, defeated, attached to the hero chosen.
capture :: PlayerId -> GameT (EnemyId, Target)
capture pid = do
  mugash <- engaged pid Enemies.mugashEnemy
  hero <- toTarget <$> getHeroWithCode "01012"
  run $ Destroy GameSource (EnemyTarget mugash)
  chooseTarget pid hero
  pure (mugash, hero)

questOn :: CardDef -> GameT Target
questOn def = do
  q <- putQuestStage def
  run $ Flip q
  pure q

spec :: Spec
spec = describe "To Catch an Orc" do
  it "is a scenario the game can be dealt" . orcTest $ \_ ->
    isJust (lookupScenario "to-catch-an-orc") `shouldBe` True

  it "sets aside an out-of-play deck of the top 20 cards with Mugash, then stages a Mountain and advances" . orcTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    deckBefore <- (.deck) <$> getPlayer pid
    _ <- setEncounterDeck [Locations.methedras, Enemies.methedrasOrc]
    chooseOptionMatching "flip the quest" pid (const True)
    resolveWhenRevealed pid
    outOfPlay <- outOfPlayDeckOf pid
    length outOfPlay `shouldBe` 21
    sort (map (.id) $ [c | c@(PlayerCard _) <- outOfPlay]) `shouldBe` sort (map (.id) $ take 20 deckBefore)
    [toCardCode c | c@(EncounterCard _) <- outOfPlay] `shouldBe` [toCardCode Enemies.mugashEnemy]
    map (.id) . (.deck) <$> getPlayer pid `shouldReturn` map (.id) (drop 20 deckBefore)
    getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` replicate 3 (toCardCode Enemies.mugashsGuard)
    placementOf Locations.methedras `shouldReturn` Just StagingArea
    quest <- getActiveQuest
    toCardCode quest `shouldBe` toCardCode Quests.searchingForMugash
    getTimeCounters (toTarget quest) `shouldReturn` 2

  it "Searches 3 as Methedras is explored: an enemy to the staging area, a card to hand, the rest discarded" . orcTest $ \pid -> do
    a <- playerCard pid ally
    b <- playerCard pid ally
    rest <- playerCard pid ally
    guard' <- encounter Enemies.mugashsGuard
    setOutOfPlayDeck pid [a, guard', b, rest]
    stage Locations.methedras
    lid <- locationOf Locations.methedras
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceTokens GameSource (toTarget lid) Progress 3
    chooseTarget pid (CardIdTarget b.id)
    placementOf Enemies.mugashsGuard `shouldReturn` Just StagingArea
    elem b.id . map (.id) . (.hand) <$> getPlayer pid `shouldReturn` True
    elem a.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` True
    map (.id) <$> outOfPlayDeckOf pid `shouldReturn` [rest.id]

  it "gives each location in the staging area +1 threat while Methedras is active" . orcTest $ \_ -> do
    stage Locations.orcCave
    threat0 <- getStagingThreat
    stage Locations.methedras
    lid <- locationOf Locations.methedras
    run $ PlaceLocation lid ActiveLocation
    getStagingThreat `shouldReturn` threat0 + 1

  it "attaches defeated Mugash to a hero, exhausting it; it cannot ready" . orcTest $ \pid -> do
    (mugash, hero) <- capture pid
    placementOf Enemies.mugashEnemy `shouldReturn` Just (AttachedTo hero)
    mugashHost `shouldReturn` Just hero
    getDamage (EnemyTarget mugash) `shouldReturn` 0
    elem CannotReady <$> getModifiers hero `shouldReturn` True
    h <- getHeroWithCode "01012"
    heroExhausted (toAttrs h) `shouldBe` True

  it "loses the game if the hero holding Mugash leaves play" . orcTest $ \pid -> do
    (_, hero) <- capture pid
    run $ Discard hero
    getsGame (.outcome) `shouldReturn` Just Lost

  it "has Mugash's Guard engage the player holding Mugash" . orcTest $ \pid -> do
    stage Enemies.mugashsGuard
    _ <- capture pid
    placementOf Enemies.mugashsGuard `shouldReturn` Just (EngagedWith pid)

  it "returns Mugash to the staging area after Mugash's Guard destroys a character" . orcTest $ \pid -> do
    stage Enemies.mugashsGuard
    _ <- capture pid
    guard' <- enemyOf Enemies.mugashsGuard
    run $ EnemyAttack pid guard'
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid =<< getHeroWithCode "01007"
    placementOf Enemies.mugashEnemy `shouldReturn` Just StagingArea

  it "shuffles a random card from the defending player's hand into his out-of-play deck as Methedras Orc attacks" . orcTest $ \pid -> do
    setOutOfPlayDeck pid []
    card <- putCardIntoHand pid ally
    g <- getGame
    putGame $ g & playersL . ix pid . handL .~ [card]
    orc <- engaged pid Enemies.methedrasOrc
    run $ CheckWindows [mkWhen $ EnemyAttacking orc pid]
    handCodes pid `shouldReturn` []
    map (.id) <$> outOfPlayDeckOf pid `shouldReturn` [card.id]

  it "reveals the top of each out-of-play deck at the beginning of the encounter phase with Mugash's Lair" . orcTest $ \pid -> do
    guard' <- encounter Enemies.mugashsGuard
    setOutOfPlayDeck pid [guard']
    stage Locations.mugashsLair
    run $ CheckWindows [mkAfter $ PhaseBegins EncounterPhase]
    placementOf Enemies.mugashsGuard `shouldReturn` Just StagingArea
    outOfPlayDeckOf pid `shouldReturn` []

  it "discards the top card of the encounter deck per player to travel to Orc Cave, staging its Orcs" . orcTest $ \_ -> do
    stage Locations.orcCave
    lid <- locationOf Locations.orcCave
    _ <- setEncounterDeck [Enemies.methedrasOrc, Locations.methedras]
    run $ TravelTo lid
    placementOf Enemies.methedrasOrc `shouldReturn` Just StagingArea
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.methedras]

  it "places a time counter on Searching for Mugash for the Response, and advances once Mugash is in play" . orcTest $ \pid -> do
    q <- questOn Quests.searchingForMugash
    prize <- genEncounterCard Quests.theWizardsPrize
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (questDeckL .~ [prize])
    run $ CheckWindows [mkAfter QuestedSuccessfully]
    chooseAbilityOf pid q
    getTimeCounters q `shouldReturn` 3
    stage Enemies.mugashEnemy
    run $ CheckWindows [mkAfter QuestedSuccessfully]
    chooseAbilityOf pid q
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.theWizardsPrize

  it "reveals 2 encounter cards per player once Searching for Mugash's last time counter goes" . orcTest $ \_ -> do
    q <- questOn Quests.searchingForMugash
    _ <- setEncounterDeck [Enemies.methedrasOrc, Locations.methedras, Locations.orcCave]
    run $ RemoveTokens GameSource q TimeCounter 2
    resolveRevealed
    placementOf Enemies.methedrasOrc `shouldReturn` Just StagingArea
    placementOf Locations.methedras `shouldReturn` Just StagingArea
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.orcCave]
    getTimeCounters q `shouldReturn` 2

  it "takes no progress on The Wizard's Prize until Mugash is attached, and wins once defeated" . orcTest $ \pid -> do
    q <- questOn Quests.theWizardsPrize
    run $ PlaceTokens GameSource q Progress 5
    (.progress) <$> getActiveQuest `shouldReturn` 0
    _ <- capture pid
    run $ PlaceTokens GameSource q Progress 15
    getsGame (.outcome) `shouldReturn` Just Won

  it "returns Mugash to the staging area once The Wizard's Prize's last time counter goes" . orcTest $ \pid -> do
    q <- questOn Quests.theWizardsPrize
    _ <- capture pid
    run $ RemoveTokens GameSource q TimeCounter 3
    placementOf Enemies.mugashEnemy `shouldReturn` Just StagingArea
    getTimeCounters q `shouldReturn` 3

  it "stages a location and has engaged Orcs attack when Orc Territory is revealed, if the quest is winning" . orcTest $ \pid -> do
    _ <- engaged pid Enemies.methedrasOrc
    -- Éowyn's 4 willpower
    hero <- toTarget <$> getHeroWithCode "01007"
    run $ CommitToQuest hero
    cards <- setEncounterDeck [Treacheries.orcTerritory, Locations.methedras]
    run RevealEncounterCard
    resolveRevealed
    for_ (drop 1 cards) \c -> chooseTarget pid (CardIdTarget c.id)
    placementOf Locations.methedras `shouldReturn` Just StagingArea
    -- the hero's willpower beats Methedras's 2 threat: the Orc attacks
    isJust <$> getQuestion pid `shouldReturn` True

  it "has no Orc attack when Orc Territory leaves the staging area's threat ahead" . orcTest $ \pid -> do
    _ <- engaged pid Enemies.methedrasOrc
    cards <- setEncounterDeck [Treacheries.orcTerritory, Locations.methedras]
    run RevealEncounterCard
    resolveRevealed
    for_ (drop 1 cards) \c -> chooseTarget pid (CardIdTarget c.id)
    placementOf Locations.methedras `shouldReturn` Just StagingArea
    isJust <$> getQuestion pid `shouldReturn` False
