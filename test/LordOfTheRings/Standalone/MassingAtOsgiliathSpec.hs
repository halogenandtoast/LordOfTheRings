module LordOfTheRings.Standalone.MassingAtOsgiliathSpec (spec) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Helpers.Attachment (cardHosts, playableCardHosts)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getEnemy, getThreat)
import LordOfTheRings.Modifier (ModifierType (GainKeyword, Defending, Blank, WillpowerModifier, CannotPlayAttachmentsOn))
import LordOfTheRings.Window (WindowType (EnemyAttacking, CommittedToQuest), mkWhen, mkAfter)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase, UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Types (encounterDiscardL)
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
