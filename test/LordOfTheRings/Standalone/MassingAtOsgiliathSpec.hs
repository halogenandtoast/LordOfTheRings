module LordOfTheRings.Standalone.MassingAtOsgiliathSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (toCard, allEncounterCards, CardDef (..), CardType (QuestType, AllyType))
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Scenario (theMassingAtOsgiliath, lookupScenario)
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Phase (Phase (TravelPhase, QuestPhase))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Helpers.Attachment (cardHosts, playableCardHosts)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getEnemy, getThreat, CardMatcher (CardWithType))
import LordOfTheRings.Modifier (ModifierType (MayPlayTopOfDeck, CannotBeDiscardedByEncounterCards, CannotPutIntoPlay, GainKeyword, Defending, Blank, WillpowerModifier, CannotPlayAttachmentsOn))
import LordOfTheRings.Window (WindowType (EnemyAttacking, CommittedToQuest), mkWhen, mkAfter)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase, UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Types (encounterDiscardL, scenarioSetAside)
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)
import TestImport hiding (before, first, second)

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "enemy missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "location missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations)

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  [card] <- setEncounterDeck [def]
  run $ DealShadowCard eid
  run $ ResolveShadowCard card.id eid pid defenders

massingDeck :: [CardDef]
massingDeck = concat
  [ replicate (fromMaybe 0 $ cdEncounterSetQuantity def) def
  | def <- Map.elems allEncounterCards
  , cdEncounterSet def == Just EncounterSet.TheMassingAtOsgiliath
  , cdCardType def /= QuestType
  ]

massingTable :: Int -> ([PlayerId] -> GameT ()) -> IO ()
massingTable size body = do
  seats <- for [1 .. size] \seat ->
    either (error . show) id <$> loadPlayer ("Player " <> tshow seat) testDeck
  let table = fromJustNote "empty test table" $ nonEmpty seats
  env <- newGameEnv (newTableIn theMassingAtOsgiliath table) (const $ pure ()) 1
  runGameT env $ body =<< getsGame (.playerOrder)

controlledRangers :: GameT [AllyAttrs]
controlledRangers = getsGame $ map toAttrs . filter ((== toCardCode Objectives.rangerOfIthilien) . toCardCode) . toList . (.entities.allies)

throughRuins :: GameT Target
throughRuins = do
  quest <- putQuestStage Quests.throughTheRuinsOsgiliath
  run $ Flip quest
  pure quest

crossing :: GameT Target
crossing = do
  quest <- putQuestStage Quests.anduinCrossing
  run $ Flip quest
  pure quest

spec :: Spec
spec = describe "The Massing at Osgiliath encounter cards" do
  it "keeps Uruk Vanguard at its printed attack before the crossing" . gameTest $ \_ -> do
    eid <- stageEnemy Enemies.urukVanguard
    getEnemyAttack eid `shouldReturn` 2
    crossedAnduin `shouldReturn` False

  it "retrieves only the top Scout enemy, skipping other encounter cards" . gameTest $ \_ -> do
    cards <- traverse genEncounterCard [Locations.oldForestRoad, Enemies.goblinScout, Enemies.urukVanguard, Enemies.goblinScout]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
    captain <- stageEnemy Enemies.wainriderCaptain
    run $ WhenRevealed (toTarget captain)
    scouts <- getsGame $ filter ((== toCardCode Enemies.goblinScout) . toCardCode) . toList . (.entities.enemies)
    length scouts `shouldBe` 1
    map ((.id) . toCard) scouts `shouldBe` map (.id) (take 1 $ drop 1 cards)
    getEncounterDiscard `shouldReturn` take 1 cards <> drop 2 cards

  it "handles a discard pile without Scouts without adding a new card" . gameTest $ \_ -> do
    cards <- traverse genEncounterCard [Locations.oldForestRoad, Enemies.urukVanguard]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
    captain <- stageEnemy Enemies.wainriderCaptain
    run $ WhenRevealed (toTarget captain)
    getEncounterDiscard `shouldReturn` cards
    getsGame (length . (.entities.enemies)) `shouldReturn` 1

  it "applies the Witch-king's willpower penalty only while he is in staging" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    beravor <- heroOf pid "01012"
    getModifiers beravor >>= (`shouldSatisfy` elem (WillpowerModifier (-1)))
    getModifiers witch >>= (`shouldSatisfy` elem CannotPlayAttachmentsOn)
    run $ PlaceEnemy witch (EngagedWith pid)
    getModifiers beravor >>= (`shouldSatisfy` notElem (WillpowerModifier (-1)))

  it "lets the defending player keep the Witch-king engaged by raising threat" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    run $ PlaceEnemy witch (EngagedWith pid)
    threat <- (.threat) <$> getPlayer pid
    run $ UseCardAbility pid (EnemySource witch) 1 [mkAfter $ EnemyAttacking witch pid]
    chooseOptionMatching "raise threat" pid \case
      Label l _ -> "$label.theWitchKingOsgiliath.threat" `isPrefixOf` l
      _ -> False
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3
    (.placement) <$> getEnemy witch `shouldReturn` EngagedWith pid

  it "returns the Witch-king to staging when the player declines the threat increase" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    run $ PlaceEnemy witch (EngagedWith pid)
    threat <- (.threat) <$> getPlayer pid
    run $ UseCardAbility pid (EnemySource witch) 1 [mkAfter $ EnemyAttacking witch pid]
    chooseOptionMatching "return to staging" pid \case
      Label l _ -> "$label.theWitchKingOsgiliath.return" `isPrefixOf` l
      _ -> False
    (.placement) <$> getEnemy witch `shouldReturn` StagingArea
    (.threat) <$> getPlayer pid `shouldReturn` threat

  it "raises the defending player's threat by the damage Wainriders actually deals" . gameTest $ \pid -> do
    riders <- stageEnemy Enemies.wainriders
    beravor <- heroOf pid "01012"
    threat <- (.threat) <$> getPlayer pid
    run $ DealDamage (EnemyAttackSource riders) beravor 2
    (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  it "deals two damage to the defending character with the Wolves shadow" . gameTest $ \pid -> do
    wolves <- stageEnemy Enemies.wolvesFromMordor
    cid <- (.id) . toCard <$> getEnemy wolves
    beravor <- heroOf pid "01012"
    run $ ResolveShadowEffect cid wolves pid [beravor]
    getDamage beravor `shouldReturn` 2

  it "shuffles Wolves back into the encounter deck after their attack destroys an ally" . gameTest $ \pid -> do
    wolves <- stageEnemy Enemies.wolvesFromMordor
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage (EnemyAttackSource wolves) (toTarget ally) 2
    run $ UseCardAbility pid (EnemySource wolves) 1 [mkAfter $ EnemyAttacking wolves pid]
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getEncounterDeck >>= (`shouldSatisfy` any ((== toCardCode Enemies.wolvesFromMordor) . toCardCode))

  it "keeps Wolves in play after an attack that destroys no character" . gameTest $ \pid -> do
    wolves <- stageEnemy Enemies.wolvesFromMordor
    run $ UseCardAbility pid (EnemySource wolves) 1 [mkAfter $ EnemyAttacking wolves pid]
    (.placement) <$> getEnemy wolves `shouldReturn` StagingArea

  it "forbids playing Forest Snare on the Witch-king while allowing it to be put into play" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    run $ PlaceEnemy witch (EngagedWith pid)
    playableCardHosts pid (toCardCode Attachments.forestSnare) >>= (`shouldSatisfy` notElem (toTarget witch))
    cardHosts pid (toCardCode Attachments.forestSnare) >>= (`shouldSatisfy` elem (toTarget witch))
    snare <- genPlayerCard Attachments.forestSnare (Just pid)
    run $ PutAttachmentIntoPlay pid snare
    attachments <- getsGame $ toList . (.entities.attachments)
    map (.placement) attachments `shouldBe` [AttachedTo $ toTarget witch]

  it "does not raise threat for damage dealt while Wainriders is blank" . gameTest $ \pid -> do
    riders <- stageEnemy Enemies.wainriders
    beravor <- heroOf pid "01012"
    threat <- (.threat) <$> getPlayer pid
    run $ CreateEffect $ Effect GameSource (toTarget riders) [Blank] UntilEndOfPhase
    run $ DealDamage (EnemyAttackSource riders) beravor 1
    getDamage beravor `shouldReturn` 1
    (.threat) <$> getPlayer pid `shouldReturn` threat

  it "raises the attacked player's threat when another player's character takes the damage" . twoPlayerTest testDeck $ \one two -> do
    riders <- stageEnemy Enemies.wainriders
    run $ PlaceEnemy riders (EngagedWith two)
    defender <- heroOf two "01012"
    beforeOne <- (.threat) <$> getPlayer one
    beforeTwo <- (.threat) <$> getPlayer two
    pushAll [DealDamage (EnemyAttackSource riders) defender 2, CheckWindows [mkAfter $ EnemyAttacking riders one]]
    void mainLoop
    (.threat) <$> getPlayer one `shouldReturn` beforeOne + 2
    (.threat) <$> getPlayer two `shouldReturn` beforeTwo

  it "moves every Snaga Scout to the lowest-threat player, including already engaged copies" . twoPlayerTest testDeck $ \one two -> do
    first <- stageEnemy Enemies.snagaScouts
    run $ PlaceEnemy first (EngagedWith two)
    second <- stageEnemy Enemies.snagaScouts
    run $ RaiseThreat two 5
    run $ UseCardAbility one (EnemySource first) 1 []
    (.placement) <$> getEnemy first `shouldReturn` EngagedWith one
    (.placement) <$> getEnemy second `shouldReturn` EngagedWith one

  it "lets the first player resolve a Snaga Scouts threat tie for all copies" . twoPlayerTest testDeck $ \one two -> do
    first <- stageEnemy Enemies.snagaScouts
    second <- stageEnemy Enemies.snagaScouts
    lead <- getLead
    run $ UseCardAbility one (EnemySource first) 1 []
    chooseTarget lead two
    (.placement) <$> getEnemy first `shouldReturn` EngagedWith two
    (.placement) <$> getEnemy second `shouldReturn` EngagedWith two

  it "changes the river-bank locations' threat on reaching stage 4" . gameTest $ \_ -> do
    tower <- stageLocation Locations.capturedWatchtower
    ruins <- stageLocation Locations.ruinsOfOsgiliath
    getLocationThreat tower `shouldReturn` 1
    getLocationThreat ruins `shouldReturn` 4
    _ <- putQuestStage Quests.raceToMinasTirith
    crossedAnduin `shouldReturn` True
    getLocationThreat tower `shouldReturn` 4
    getLocationThreat ruins `shouldReturn` 1

  it "raises Uruk Vanguard's attack after the crossing" . gameTest $ \_ -> do
    eid <- stageEnemy Enemies.urukVanguard
    _ <- putQuestStage Quests.raceToMinasTirith
    getEnemyAttack eid `shouldReturn` 5

  it "retrieves the top two Scouts after the crossing, skipping other cards" . gameTest $ \_ -> do
    _ <- putQuestStage Quests.raceToMinasTirith
    cards <- traverse genEncounterCard [Locations.oldForestRoad, Enemies.snagaScouts, Enemies.urukVanguard, Enemies.wainriders, Enemies.wolvesFromMordor]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
    captain <- stageEnemy Enemies.wainriderCaptain
    run $ WhenRevealed (toTarget captain)
    getEncounterDiscard `shouldReturn` take 1 cards <> take 1 (drop 2 cards) <> drop 4 cards

  it "gives the first revealed Scout surge and doomed 2, but not the second" . gameTest $ \pid -> do
    _ <- stageLocation Locations.emynArnenOverlook
    before <- getThreat pid
    reveal [Enemies.snagaScouts, Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 2
    getsGame (length . (.entities.locations)) `shouldReturn` 2
    [_, remaining] <- setEncounterDeck [Enemies.snagaScouts, Locations.capturedWatchtower]
    run RevealEncounterCard
    resolveRevealed
    getThreat pid `shouldReturn` before + 2
    getEncounterDeck `shouldReturn` [remaining]

  it "does not count a Scout added to staging as a revealed Scout" . gameTest $ \pid -> do
    _ <- stageLocation Locations.emynArnenOverlook
    _ <- stageEnemy Enemies.snagaScouts
    before <- getThreat pid
    reveal [Enemies.wainriders, Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 2

  it "remembers Scouts revealed earlier in the round before the Overlook entered play" . gameTest $ \pid -> do
    reveal [Enemies.snagaScouts]
    _ <- stageLocation Locations.emynArnenOverlook
    before <- getThreat pid
    [_, remaining] <- setEncounterDeck [Enemies.wainriders, Locations.capturedWatchtower]
    run RevealEncounterCard
    resolveRevealed
    getThreat pid `shouldReturn` before
    getEncounterDeck `shouldReturn` [remaining]

  it "resets the Overlook's first-Scout effect when round effects expire" . gameTest $ \pid -> do
    _ <- stageLocation Locations.emynArnenOverlook
    reveal [Enemies.snagaScouts, Locations.capturedWatchtower]
    run $ EndEffects UntilEndOfRound
    before <- getThreat pid
    reveal [Enemies.wainriders, Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 2

  it "makes Massing's surge card doomed without giving Massing its own doomed" . gameTest $ \pid -> do
    before <- getThreat pid
    reveal [Treacheries.massingAtOsgiliath, Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 1

  it "stacks multiple Massing effects and snapshots doomed before each When Revealed" . gameTest $ \pid -> do
    before <- getThreat pid
    reveal [Treacheries.massingAtOsgiliath, Treacheries.massingAtOsgiliath, Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 3

  it "uses doomed 3 after the river crossing and expires it at the end of the phase" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.raceToMinasTirith
    before <- getThreat pid
    reveal [Treacheries.massingAtOsgiliath, Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 3
    run $ EndEffects UntilEndOfPhase
    reveal [Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 3

  it "counts engaged Scouts for Dark Pursuit and removes its threat at phase end" . gameTest $ \pid -> do
    scout <- stageEnemy Enemies.snagaScouts
    run $ PlaceEnemy scout (EngagedWith pid)
    _ <- stageEnemy Enemies.wainriders
    before <- getStagingThreat
    reveal [Treacheries.darkPursuit]
    getStagingThreat `shouldReturn` before + 2
    run $ EndEffects UntilEndOfPhase
    getStagingThreat `shouldReturn` before

  it "surges Dark Pursuit when no Scouts are in play" . gameTest $ \_ -> do
    reveal [Treacheries.darkPursuit, Locations.capturedWatchtower]
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "discards allies from each player's hand with Cut Off, keeping other card types" . twoPlayerTest testDeck $ \one two -> do
    _ <- putCardIntoHand one Allies.guardOfTheCitadel
    _ <- putCardIntoHand two Allies.guardOfTheCitadel
    attachment <- putCardIntoHand one Attachments.forestSnare
    reveal [Treacheries.cutOffOsgiliath]
    (.hand) <$> getPlayer one `shouldReturn` [attachment]
    (.hand) <$> getPlayer two `shouldReturn` []

  it "requires two allies for Cut Off's undefended shadow and offers the attack increase otherwise" . gameTest $ \pid -> do
    scout <- stageEnemy Enemies.snagaScouts
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    shadow pid scout Treacheries.cutOffOsgiliath []
    chooseOptionMatching "increase attack" pid \case
      Label l _ -> "$label.cutOffOsgiliath.attack" `isPrefixOf` l
      _ -> False
    getEnemyAttack scout `shouldReturn` 4
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "allows discarding exactly two allies to avoid Cut Off's undefended attack increase" . gameTest $ \pid -> do
    scout <- stageEnemy Enemies.snagaScouts
    first <- putCardIntoHand pid Allies.guardOfTheCitadel
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    shadow pid scout Treacheries.cutOffOsgiliath []
    chooseOptionMatching "discard allies" pid \case
      Label l _ -> "$label.cutOffOsgiliath.discard" `isPrefixOf` l
      _ -> False
    chooseTarget pid (CardIdTarget first.id)
    (.hand) <$> getPlayer pid `shouldReturn` []
    getEnemyAttack scout `shouldReturn` 1

  it "damages a character committing to the quest only while Morgulduin is active" . gameTest $ \pid -> do
    river <- stageLocation Locations.morgulduin
    beravor <- heroOf pid "01012"
    run $ CheckWindows [mkWhen $ CommittedToQuest beravor]
    getDamage beravor `shouldReturn` 0
    run $ PlaceLocation river ActiveLocation
    run $ CheckWindows [mkWhen $ CommittedToQuest beravor]
    getDamage beravor `shouldReturn` 1
    run $ PlaceLocation river StagingArea
    run $ CheckWindows [mkWhen $ CommittedToQuest beravor]
    getDamage beravor `shouldReturn` 1

  it "makes Morgulduin's shadow replace the active location before the crossing" . gameTest $ \pid -> do
    old <- stageLocation Locations.capturedWatchtower
    run $ PlaceLocation old ActiveLocation
    scout <- stageEnemy Enemies.snagaScouts
    shadow pid scout Locations.morgulduin []
    (.placement) <$> getLocation old `shouldReturn` StagingArea
    active <- getsGame $ filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations)
    map toCardCode active `shouldBe` [toCardCode Locations.morgulduin]

  it "does not replace the active location with Morgulduin after the crossing" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.raceToMinasTirith
    old <- stageLocation Locations.capturedWatchtower
    run $ PlaceLocation old ActiveLocation
    scout <- stageEnemy Enemies.snagaScouts
    shadow pid scout Locations.morgulduin []
    (.placement) <$> getLocation old `shouldReturn` ActiveLocation
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "charges each player for each Pelennor Fields they decline to travel to" . twoPlayerTest testDeck $ \one two -> do
    _ <- putQuestStage Quests.raceToMinasTirith
    _ <- stageLocation Locations.pelennorFieldsOsgiliath
    _ <- stageLocation Locations.pelennorFieldsOsgiliath
    beforeOne <- getThreat one
    beforeTwo <- getThreat two
    run $ DoStep 1 (BeginPhase TravelPhase)
    lead <- getLead
    chooseOptionMatching "do not travel" lead \case
      Label l _ -> "$label.doNotTravel" `isPrefixOf` l
      _ -> False
    getThreat one `shouldReturn` beforeOne + 6
    getThreat two `shouldReturn` beforeTwo + 6

  it "waives the penalty only for the Pelennor Fields chosen as the destination" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.raceToMinasTirith
    first <- stageLocation Locations.pelennorFieldsOsgiliath
    _ <- stageLocation Locations.pelennorFieldsOsgiliath
    before <- getThreat pid
    run $ DoStep 1 (BeginPhase TravelPhase)
    chooseTarget pid first
    getThreat pid `shouldReturn` before + 3
    (.placement) <$> getLocation first `shouldReturn` ActiveLocation

  it "does not charge Pelennor Fields' penalty before the crossing" . gameTest $ \pid -> do
    _ <- stageLocation Locations.pelennorFieldsOsgiliath
    before <- getThreat pid
    run $ DoStep 1 (BeginPhase TravelPhase)
    chooseOptionMatching "do not travel" pid \case
      Label l _ -> "$label.doNotTravel" `isPrefixOf` l
      _ -> False
    getThreat pid `shouldReturn` before

  it "does not charge a travel penalty while another active location prevents travel" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.raceToMinasTirith
    _ <- stageLocation Locations.pelennorFieldsOsgiliath
    active <- stageLocation Locations.capturedWatchtower
    run $ PlaceLocation active ActiveLocation
    before <- getThreat pid
    run $ DoStep 1 (BeginPhase TravelPhase)
    getQuestion pid `shouldReturn` Nothing
    getThreat pid `shouldReturn` before

  it "combines the Overlook's doomed 2 with Massing's doomed 1 on the first Scout" . gameTest $ \pid -> do
    _ <- stageLocation Locations.emynArnenOverlook
    before <- getThreat pid
    reveal [Treacheries.massingAtOsgiliath, Enemies.snagaScouts, Locations.capturedWatchtower]
    getThreat pid `shouldReturn` before + 4

  it "grants the doomed keyword to a revealed location for Massing's duration" . gameTest $ \_ -> do
    reveal [Treacheries.massingAtOsgiliath, Locations.capturedWatchtower]
    location <- getsGame $ toId . fromJustNote "no location" . headMay . toList . (.entities.locations)
    getModifiers location >>= (`shouldSatisfy` elem (GainKeyword $ Doomed 1))
    run $ EndEffects UntilEndOfPhase
    getModifiers location >>= (`shouldSatisfy` notElem (GainKeyword $ Doomed 1))

  it "adds the set-aside Witch-king and blocks only East Bank travel on the final stage" . gameTest $ \_ -> do
    card <- genEncounterCard Enemies.theWitchKingOsgiliath
    run $ SetAside card
    east <- stageLocation Locations.ruinsOfOsgiliath
    west <- stageLocation Locations.capturedWatchtower
    quest <- putQuestStage Quests.raceToMinasTirith
    run $ Flip quest
    resolveRevealed
    travelableLocations >>= (`shouldSatisfy` \ls -> west `elem` ls && east `notElem` ls)
    enemies <- getsGame $ map toCardCode . toList . (.entities.enemies)
    enemies `shouldBe` [toCardCode Enemies.theWitchKingOsgiliath]
    run $ PlaceTokens GameSource quest Progress 15
    getsGame (.outcome) `shouldReturn` Just Won

  it "removes the defender and resolves Captured Watchtower's attack as undefended" . gameTest $ \pid -> do
    scout <- stageEnemy Enemies.snagaScouts
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    beravor <- heroOf pid "01012"
    run $ CreateEffect $ Effect (EnemyAttackSource scout) defender [Defending] UntilEndOfAttack
    [card] <- setEncounterDeck [Locations.capturedWatchtower]
    run $ DealShadowCard scout
    pushAll [ResolveShadowCard card.id scout pid [defender], AttackDamage scout defender, CheckWindows [mkAfter $ EnemyAttacking scout pid]]
    void mainLoop
    chooseTarget pid beravor
    getDamage defender `shouldReturn` 0
    getDamage beravor `shouldReturn` 1
    getModifiers defender >>= (`shouldSatisfy` notElem Defending)

  it "allows discarding one ally for Cut Off's defended shadow" . gameTest $ \pid -> do
    scout <- stageEnemy Enemies.snagaScouts
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    beravor <- heroOf pid "01012"
    shadow pid scout Treacheries.cutOffOsgiliath [beravor]
    chooseOptionMatching "discard an ally" pid \case
      Label l _ -> "$label.cutOffOsgiliath.discard" `isPrefixOf` l
      _ -> False
    (.hand) <$> getPlayer pid `shouldReturn` []
    getEnemyAttack scout `shouldReturn` 1

  it "defines all 41 encounter cards in the Osgiliath encounter set"
    (length massingDeck `shouldBe` 41 :: IO ())

  for_ [1 .. 4] \size ->
    it ("sets up exactly three Scouts per player at a table of " <> show size) . massingTable size $ \_ -> do
      _ <- setEncounterDeck massingDeck
      quest <- putQuestStage Quests.beyondExpectations
      run $ PerformSetup quest
      g <- getGame
      let scouts = [Enemies.snagaScouts, Enemies.wolvesFromMordor, Enemies.wainriders]
          foes = toList g.entities.enemies
          remaining = (toAttrs g.scenario).encounterDeck
      length foes `shouldBe` 3 * size
      for_ scouts \def -> do
        length (filter ((== toCardCode def) . toCardCode) foes) `shouldBe` size
        length (filter ((== toCardCode def) . toCardCode) remaining) `shouldBe` 4 - size
      length remaining `shouldBe` 40 - 3 * size
      map toCardCode (scenarioSetAside $ toAttrs g.scenario) `shouldBe` [toCardCode Enemies.theWitchKingOsgiliath]
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

  it "forbids West Bank travel on Beyond Expectations while allowing East Bank travel" . gameTest $ \_ -> do
    west <- stageLocation Locations.capturedWatchtower
    east <- stageLocation Locations.morgulduin
    quest <- putQuestStage Quests.beyondExpectations
    run $ Flip quest
    travelableLocations >>= (`shouldSatisfy` \ls -> east `elem` ls && west `notElem` ls)

  it "reveals Ranger of Ithilien exhausted and committed under the first player, then surges" . twoPlayerTest testDeck $ \_ _ -> do
    lead <- getLead
    reveal [Objectives.rangerOfIthilien, Locations.capturedWatchtower]
    [ranger] <- controlledRangers
    ranger.placement `shouldBe` InPlayArea lead
    ranger.allyExhausted `shouldBe` True
    ranger.allyCommitted `shouldBe` True
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "can reveal both non-unique Rangers without violating the unique rule" . gameTest $ \_ -> do
    reveal [Objectives.rangerOfIthilien, Objectives.rangerOfIthilien, Locations.capturedWatchtower]
    length <$> controlledRangers `shouldReturn` 2

  it "does not trigger Morgulduin's commitment damage for a Ranger entering committed" . gameTest $ \_ -> do
    river <- stageLocation Locations.morgulduin
    run $ PlaceLocation river ActiveLocation
    reveal [Objectives.rangerOfIthilien, Locations.capturedWatchtower]
    [ranger] <- controlledRangers
    getDamage (toTarget ranger) `shouldReturn` 0

  it "deals Ranger's shadow damage before the defending player exhausts a character to claim it" . twoPlayerTest testDeck $ \_ two -> do
    attacker <- stageEnemy Enemies.urukVanguard
    beravor <- heroOf two "01012"
    shadow two attacker Objectives.rangerOfIthilien []
    getDamage (toTarget attacker) `shouldReturn` 2
    chooseTarget two beravor
    [ranger] <- controlledRangers
    ranger.placement `shouldBe` InPlayArea two
    ranger.allyExhausted `shouldBe` False
    ranger.allyCommitted `shouldBe` False
    -- The first player's Beravor stays ready; the defending player's was exhausted.
    characters <- getsGame $ filter ((== "01012") . toCardCode) . toList . (.entities.heroes)
    map ((.heroExhausted) . toAttrs) [h | h <- characters, h.owner == two] `shouldBe` [True]
    map ((.heroExhausted) . toAttrs) [h | h <- characters, h.owner /= two] `shouldBe` [False]
    null . (.shadowCards) <$> getEnemy attacker `shouldReturn` True

  it "can claim Ranger's shadow after its damage destroys the attacker" . gameTest $ \pid -> do
    attacker <- stageEnemy Enemies.snagaScouts
    beravor <- heroOf pid "01012"
    shadow pid attacker Objectives.rangerOfIthilien []
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    chooseTarget pid beravor
    [ranger] <- controlledRangers
    ranger.placement `shouldBe` InPlayArea pid
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.snagaScouts]

  it "allows declining Ranger's shadow after dealing the damage" . gameTest $ \pid -> do
    attacker <- stageEnemy Enemies.urukVanguard
    shadow pid attacker Objectives.rangerOfIthilien []
    chooseOptionMatching "decline the Ranger" pid \case
      Label l _ -> "$label.rangerOfIthilien.decline" `isPrefixOf` l
      _ -> False
    controlledRangers `shouldReturn` []
    getDamage (toTarget attacker) `shouldReturn` 2

  it "cannot claim Ranger's shadow without a character that can exhaust" . gameTest $ \pid -> do
    heroes <- getsGame $ map toTarget . toList . (.entities.heroes)
    for_ heroes (run . Exhaust)
    attacker <- stageEnemy Enemies.urukVanguard
    shadow pid attacker Objectives.rangerOfIthilien []
    chooseOptionMatching "decline with no ready characters" pid \case
      Label l _ -> "$label.rangerOfIthilien.decline" `isPrefixOf` l
      _ -> False
    controlledRangers `shouldReturn` []

  it "resolves Morgulduin when a hero actually commits to the quest" . gameTest $ \pid -> do
    river <- stageLocation Locations.morgulduin
    run $ PlaceLocation river ActiveLocation
    beravor <- heroOf pid "01012"
    run $ CommitToQuest beravor
    getDamage beravor `shouldReturn` 1
    (.heroCommitted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "does not claim or surge Ranger when its When Revealed effect is canceled" . gameTest $ \pid -> do
    cancellation <- putCardIntoHand pid Events.aTestOfWill
    eowyn <- heroOf pid "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    [_, nextCard] <- setEncounterDeck [Objectives.rangerOfIthilien, Locations.capturedWatchtower]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget cancellation.id)
    resolveRevealed
    controlledRangers `shouldReturn` []
    getEncounterDeck `shouldReturn` [nextCard]

  it "allows only one paid play from each player's hand per round at stage 2" . gameTest $ \pid -> do
    _ <- throughRuins
    theodred <- heroOf pid "01002"
    run $ PlaceTokens GameSource theodred Resource 4
    firstCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    secondCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PlayCard pid firstCard.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    player <- getPlayer pid
    canPlay player secondCard `shouldReturn` False
    run $ PlayCard pid secondCard.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [secondCard.id]

  it "shares the stage 2 allowance between putting an ally into play and playing a card" . gameTest $ \pid -> do
    _ <- throughRuins
    theodred <- heroOf pid "01002"
    run $ PlaceTokens GameSource theodred Resource 4
    firstCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    secondCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid firstCard.id
    run $ PlayCard pid secondCard.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [secondCard.id]

  it "tracks cards used earlier in the round before reaching Through the Ruins" . gameTest $ \pid -> do
    firstCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid firstCard.id
    _ <- throughRuins
    secondCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid secondCard.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [secondCard.id]

  it "gives each player a separate allowance at stage 2" . twoPlayerTest testDeck $ \one two -> do
    _ <- throughRuins
    cardOne <- putCardIntoHand one Allies.guardOfTheCitadel
    cardTwo <- putCardIntoHand two Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay one cardOne.id
    run $ PutAllyIntoPlay two cardTwo.id
    getsGame (length . (.entities.allies)) `shouldReturn` 2

  it "resets the hand allowance at the end of the round" . gameTest $ \pid -> do
    _ <- throughRuins
    firstCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    secondCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid firstCard.id
    run $ EndEffects UntilEndOfRound
    run $ PutAllyIntoPlay pid secondCard.id
    getsGame (length . (.entities.allies)) `shouldReturn` 2

  it "counts an attachment put from hand against subsequent ally entry" . gameTest $ \pid -> do
    _ <- throughRuins
    beravor <- heroOf pid "01012"
    attachment <- putCardIntoHand pid Attachments.stewardOfGondor
    run $ PutAttachmentIntoPlayFromHand pid attachment.id beravor
    ally <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid ally.id
    getsGame (length . (.entities.attachments)) `shouldReturn` 1
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [ally.id]

  it "blocks a second attachment put from hand without removing it" . gameTest $ \pid -> do
    _ <- throughRuins
    ally <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid ally.id
    attachment <- putCardIntoHand pid Attachments.stewardOfGondor
    beravor <- heroOf pid "01012"
    run $ PutAttachmentIntoPlayFromHand pid attachment.id beravor
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [attachment.id]

  it "counts a card played from hand for no cost exactly once" . gameTest $ \pid -> do
    _ <- throughRuins
    firstCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    secondCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PlayFromHandAtNoCost pid firstCard.id
    run $ PlayFromHandAtNoCost pid secondCard.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [secondCard.id]

  it "permits out-of-play allies after the hand allowance is used" . gameTest $ \pid -> do
    _ <- throughRuins
    firstCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid firstCard.id
    card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
    run $ PutAllyIntoPlayUnder pid card
    getsGame (length . (.entities.allies)) `shouldReturn` 2

  it "does not spend the hand allowance on a prevented ally entry" . gameTest $ \pid -> do
    _ <- throughRuins
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotPutIntoPlay $ CardWithType AllyType] UntilEndOfPhase
    card <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid card.id
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    run $ EndEffects UntilEndOfPhase
    run $ PutAllyIntoPlay pid card.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1

  it "counts Sneak Attack itself before its attempt to put an ally from hand" . gameTest $ \pid -> do
    _ <- throughRuins
    theodred <- heroOf pid "01002"
    run $ PlaceTokens GameSource theodred Resource 1
    ally <- putCardIntoHand pid Allies.guardOfTheCitadel
    event <- putCardIntoHand pid Events.sneakAttack
    run $ PlayCard pid event.id
    chooseTarget pid (CardIdTarget ally.id)
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [ally.id]

  it "keeps the West Bank travel restriction at stage 2" . gameTest $ \_ -> do
    west <- stageLocation Locations.capturedWatchtower
    east <- stageLocation Locations.morgulduin
    _ <- throughRuins
    travelableLocations >>= (`shouldSatisfy` \ls -> east `elem` ls && west `notElem` ls)

  it "requires a hero discard before the first declaration, once for all that player's characters" . gameTest $ \pid -> do
    _ <- crossing
    beravor <- heroOf pid "01012"
    theodred <- heroOf pid "01002"
    eowyn <- heroOf pid "01007"
    run $ CommitToQuest beravor
    chooseTarget pid theodred
    getsGame (length . (.entities.heroes)) `shouldReturn` 2
    (.heroCommitted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    run $ CommitToQuest eowyn
    getQuestion pid `shouldReturn` Nothing
    (.heroCommitted) . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
    getsGame (length . (.entities.heroes)) `shouldReturn` 2

  it "allows an exhausted Ranger of Ithilien to be discarded instead of a hero" . gameTest $ \pid -> do
    reveal [Objectives.rangerOfIthilien, Locations.capturedWatchtower]
    [ranger] <- controlledRangers
    _ <- crossing
    beravor <- heroOf pid "01012"
    run $ CommitToQuest beravor
    chooseTarget pid ranger
    controlledRangers `shouldReturn` []
    getsGame (length . (.entities.heroes)) `shouldReturn` 3
    (.heroCommitted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "does not offer an ordinary ally or the intended committer as the sacrifice" . gameTest $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- crossing
    beravor <- heroOf pid "01012"
    theodred <- heroOf pid "01002"
    run $ CommitToQuest beravor
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` \ts -> ally `notElem` ts && beravor `notElem` ts
    chooseTarget pid theodred

  it "requires each player to pay their own commitment cost" . twoPlayerTest testDeck $ \one two -> do
    _ <- crossing
    for_ [one, two] \pid -> do
      beravor <- heroOf pid "01012"
      theodred <- heroOf pid "01002"
      run $ CommitToQuest beravor
      chooseTarget pid theodred
    getsGame (length . (.entities.heroes)) `shouldReturn` 4

  it "requires a new sacrifice in the next quest phase" . gameTest $ \pid -> do
    _ <- crossing
    beravor <- heroOf pid "01012"
    theodred <- heroOf pid "01002"
    eowyn <- heroOf pid "01007"
    run $ CommitToQuest beravor
    chooseTarget pid theodred
    run $ EndEffects UntilEndOfPhase
    run $ RemoveFromQuest beravor
    run $ Ready beravor
    run $ CommitToQuest beravor
    chooseTarget pid eowyn
    getsGame (length . (.entities.heroes)) `shouldReturn` 1

  it "pays the sacrifice before Morgulduin damages the committing character" . gameTest $ \pid -> do
    _ <- crossing
    river <- stageLocation Locations.morgulduin
    run $ PlaceLocation river ActiveLocation
    beravor <- heroOf pid "01012"
    theodred <- heroOf pid "01002"
    run $ CommitToQuest beravor
    getDamage beravor `shouldReturn` 0
    chooseTarget pid theodred
    getDamage beravor `shouldReturn` 1
    (.heroCommitted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "does not charge a sacrifice for Ranger entering play already committed" . gameTest $ \_ -> do
    _ <- crossing
    reveal [Objectives.rangerOfIthilien, Locations.capturedWatchtower]
    [ranger] <- controlledRangers
    ranger.allyCommitted `shouldBe` True
    getsGame (length . (.entities.heroes)) `shouldReturn` 3

  it "charges the sacrifice for an exhausted character committed by Late Adventurer" . gameTest $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- crossing
    putGame . (phaseL .~ QuestPhase) =<< getGame
    event <- putCardIntoHand pid Events.lateAdventurer
    theodred <- heroOf pid "01002"
    run $ PlayFromHandAtNoCost pid event.id
    chooseTarget pid ally
    chooseTarget pid theodred
    allies <- getsGame $ toList . (.entities.allies)
    map ((.allyCommitted) . toAttrs) allies `shouldBe` [True]

  it "does not allow commitments when no other character can be discarded for the cost" . gameTest $ \pid -> do
    _ <- crossing
    heroes <- getsGame $ map toTarget . toList . (.entities.heroes)
    for_ heroes \hero -> run $ CreateEffect $ Effect GameSource hero [CannotBeDiscardedByEncounterCards] UntilEndOfPhase
    beravor <- heroOf pid "01012"
    run $ CommitCharacters pid
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldBe` []
    chooseOptionMatching "done committing" pid \case
      Label l _ -> "$label.doneCommitting" `isPrefixOf` l
      _ -> False
    run $ CommitToQuest beravor
    getQuestion pid `shouldReturn` Nothing
    (.heroCommitted) . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it "blocks card-effect progress on the crossing and active location but not other staged locations" . gameTest $ \pid -> do
    quest <- crossing
    active <- stageLocation Locations.capturedWatchtower
    staged <- stageLocation Locations.ruinsOfOsgiliath
    run $ PlaceLocation active ActiveLocation
    beravor <- heroOf pid "01012"
    let source = case beravor of HeroTarget hid -> HeroSource hid; _ -> GameSource
    run $ PlaceTokens source quest Progress 1
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0
    run $ PlaceTokens source (toTarget active) Progress 1
    qid <- toId <$> getActiveQuest
    run $ PlaceTokens (QuestSource qid) (toTarget active) Progress 1
    (.progress) <$> getLocation active `shouldReturn` 0
    run $ PlaceTokens source (toTarget staged) Progress 1
    (.progress) <$> getLocation staged `shouldReturn` 1
    run $ PlaceTokens GameSource (toTarget active) Progress 1
    (.progress) <$> getLocation active `shouldReturn` 1

  it "blocks travel to both banks before completing the crossing" . gameTest $ \_ -> do
    east <- stageLocation Locations.morgulduin
    west <- stageLocation Locations.capturedWatchtower
    road <- stageLocation Locations.oldForestRoad
    _ <- crossing
    travelableLocations >>= (`shouldSatisfy` \ls -> road `elem` ls && east `notElem` ls && west `notElem` ls)
    crossedAnduin `shouldReturn` False

  it "is available in the scenario registry and builds all four quest stages" . gameTestIn theMassingAtOsgiliath testDeck $ \pid -> do
    isJust (lookupScenario "the-massing-at-osgiliath") `shouldBe` True
    flipFirstQuest pid
    g <- getGame
    map toCardCode ((toAttrs g.scenario).questDeck) `shouldBe` map toCardCode [Quests.throughTheRuinsOsgiliath, Quests.anduinCrossing, Quests.raceToMinasTirith]
    length (toList g.entities.enemies) `shouldBe` 3
    length ((toAttrs g.scenario).encounterDeck) `shouldBe` 37

  for_ [1 .. 4] \size ->
    it ("runs the complete Osgiliath setup pipeline for " <> show size <> " players") . massingTable size $ \players -> do
      run Setup
      for_ players \_ -> do
        askedPlayers <- getsGame (Map.keys . (.question))
        let asked = fromJustNote "no setup question" $ headMay askedPlayers
        chooseLabel "$label.keepHand" asked
      lead <- getLead
      chooseOptionMatching "perform Beyond Expectations setup" lead (const True)
      g <- getGame
      length (toList g.entities.enemies) `shouldBe` 3 * size
      length ((toAttrs g.scenario).encounterDeck) `shouldBe` 40 - 3 * size
      map toCardCode ((toAttrs g.scenario).questDeck) `shouldBe` map toCardCode [Quests.throughTheRuinsOsgiliath, Quests.anduinCrossing, Quests.raceToMinasTirith]

  it "advances through all four stages and wins after crossing the Anduin" . gameTestIn theMassingAtOsgiliath testDeck $ \pid -> do
    run $ DoStep 1 Setup
    opening <- toTarget <$> getActiveQuest
    run $ PerformSetup opening
    run $ PlaceTokens GameSource opening Progress 7
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.throughTheRuinsOsgiliath
    ruins <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource ruins Progress 5
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.anduinCrossing
    crossedAnduin `shouldReturn` False
    putGame . (phaseL .~ QuestPhase) =<< getGame
    beravor <- heroOf pid "01012"
    theodred <- heroOf pid "01002"
    run $ CreateEffect $ Effect GameSource beravor [WillpowerModifier 100] UntilEndOfPhase
    run $ CommitToQuest beravor
    chooseTarget pid theodred
    run QuestResolution
    resolveRevealed
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.raceToMinasTirith
    crossedAnduin `shouldReturn` True
    foes <- getsGame $ map toCardCode . toList . (.entities.enemies)
    foes `shouldSatisfy` elem (toCardCode Enemies.theWitchKingOsgiliath)
    finalStage <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource finalStage Progress 15
    getsGame (.outcome) `shouldReturn` Just Won

  it "adds an explored Pelennor Fields to the victory display instead of the encounter discard pile" . gameTest $ \_ -> do
    fields <- stageLocation Locations.pelennorFieldsOsgiliath
    run $ PlaceLocation fields ActiveLocation
    run $ PlaceTokens GameSource (toTarget fields) Progress 7
    won <- getsGame $ map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)
    won `shouldBe` [toCardCode Locations.pelennorFieldsOsgiliath]
    map toCardCode <$> getEncounterDiscard `shouldReturn` []

  it "allows an out-of-play card to be played after the hand allowance is used" . gameTest $ \pid -> do
    _ <- throughRuins
    firstCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid firstCard.id
    card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
    run $ PlayAtNoCost pid card
    getsGame (length . (.entities.allies)) `shouldReturn` 2

  it "counts a top-of-deck card played as if in hand against the allowance" . gameTest $ \pid -> do
    _ <- throughRuins
    card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ [PlayerCard card]
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [MayPlayTopOfDeck] UntilEndOfPhase
    theodred <- heroOf pid "01002"
    run $ PlaceTokens GameSource theodred Resource 2
    run $ PlayCard pid card.id
    handCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid handCard.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [handCard.id]

  it "blocks top-of-deck plays as if in hand once the allowance is used" . gameTest $ \pid -> do
    _ <- throughRuins
    handCard <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ PutAllyIntoPlay pid handCard.id
    card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ [PlayerCard card]
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [MayPlayTopOfDeck] UntilEndOfPhase
    theodred <- heroOf pid "01002"
    run $ PlaceTokens GameSource theodred Resource 2
    run $ PlayCard pid card.id
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    map (.id) . (.deck) <$> getPlayer pid `shouldReturn` [card.id]
