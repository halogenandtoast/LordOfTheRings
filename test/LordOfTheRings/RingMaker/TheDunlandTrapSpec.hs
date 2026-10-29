module LordOfTheRings.RingMaker.TheDunlandTrapSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getTimeCounters, getTimeX)
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, RefreshPhase, ResourcePhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theDunlandTrap)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (PhaseEnds))
import TestImport

trapTest :: (PlayerId -> GameT ()) -> IO ()
trapTest = gameTestIn theDunlandTrap testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  run . AddToStaging =<< genEncounterCard def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

staged :: CardDef -> GameT LocationId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

active :: CardDef -> GameT LocationId
active def = do
  run . PutIntoPlayAsActiveLocation =<< genEncounterCard def
  locationOf def

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

-- | Pick the first card the open search offers.
pickFirst :: PlayerId -> GameT ()
pickFirst pid = chooseOptionMatching "take a card" pid \case
  TargetLabel {} -> True
  _ -> False

-- | Resolve each Forced effect the lead is asked to order.
resolveForced :: PlayerId -> GameT ()
resolveForced pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> chooseAbility pid >> resolveForced pid
    _ -> pure ()

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

spec :: Spec
spec = describe "The Dunland Trap" do
  it "is a scenario the game can be dealt" . trapTest $ \_ ->
    isJust (lookupScenario "the-dunland-trap") `shouldBe` True

  it "sets Chief Turch and Munuv Dûv Ravine aside, makes Old South Road active and engages a Boar Clan enemy" . trapTest $ \pid -> do
    flipFirstQuest pid
    pickFirst pid
    placementOf Locations.oldSouthRoad `shouldReturn` Just ActiveLocation
    deck <- map toCardCode <$> getEncounterDeck
    for_ [Enemies.chiefTurchTheDunlandTrap, Locations.munuvDuvRavine, Locations.oldSouthRoad] \c ->
      deck `shouldSatisfy` notElem (toCardCode c)
    g <- getGame
    [e | e <- toList g.entities.enemies, e.placement == EngagedWith pid] `shouldSatisfy` ((== 1) . length)
    q <- toTarget <$> getActiveQuest
    getTimeCounters q `shouldReturn` 2

  it "discards each hand for 2 new cards once The Road to Tharbad's last time counter is removed" . trapTest $ \pid -> do
    q <- putQuestStage Quests.theRoadToTharbad
    run $ PlaceTokens GameSource q TimeCounter 1
    run $ DrawCards $ Drawing pid 5
    run $ RemoveTokens GameSource q TimeCounter 1
    handSize pid `shouldReturn` 2
    getTimeCounters q `shouldReturn` 2

  it "removes a time counter from the current quest at the end of the refresh phase with Old South Road" . trapTest $ \_ -> do
    q <- putQuestStage Quests.theRoadToTharbad
    run $ PlaceTokens GameSource q TimeCounter 2
    _ <- staged Locations.oldSouthRoad
    run $ CheckWindows [Window When (PhaseEnds RefreshPhase)]
    getTimeCounters q `shouldReturn` 1

  it "gives Boar Clan Stalker a resource as the engaged player draws, +1 attack and defense for each up to 4" . trapTest $ \pid -> do
    stalker <- engaged pid Enemies.boarClanStalker
    run $ DrawCards $ Drawing pid 2
    getEnemyAttack stalker `shouldReturn` 1
    getEnemyDefense stalker `shouldReturn` 1
    run $ PlaceTokens GameSource (toTarget stalker) Resource 5
    getEnemyAttack stalker `shouldReturn` 4

  it "deals Boar Clan Warrior a shadow card as the engaged player draws" . trapTest $ \pid -> do
    warrior <- engaged pid Enemies.boarClanWarrior
    _ <- setEncounterDeck [Locations.plainsOfEnedwaith]
    run $ DrawCards $ Drawing pid 1
    length . (.shadowCards) <$> getEnemy warrior `shouldReturn` 1

  it "engages a Dunland enemy Hills of Dunland discards as a player draws" . trapTest $ \pid -> do
    _ <- staged Locations.hillsOfDunland
    _ <- setEncounterDeck [Enemies.boarClanStalker]
    run $ DrawCards $ Drawing pid 1
    placementOf Enemies.boarClanStalker `shouldReturn` Just (EngagedWith pid)

  it "does not stack Hills of Dunland" . trapTest $ \pid -> do
    _ <- staged Locations.hillsOfDunland
    run . AddToStaging =<< genEncounterCard Locations.hillsOfDunland
    _ <- setEncounterDeck [Locations.plainsOfEnedwaith, Locations.plainsOfEnedwaith]
    run $ DrawCards $ Drawing pid 1
    -- the lead orders the two copies' Forced effects
    replicateM_ 2 $ chooseAbility pid
    length <$> getEncounterDiscard `shouldReturn` 1

  it "has each player draw to travel to Hills of Dunland" . trapTest $ \pid -> do
    hills <- staged Locations.hillsOfDunland
    _ <- setEncounterDeck [Locations.plainsOfEnedwaith]
    drawn <- handSize pid
    run $ TravelTo hills
    handSize pid `shouldReturn` drawn + 1

  it "gives Hithaeglir Foothills a threat for each resource placed as players draw" . trapTest $ \pid -> do
    foothills <- staged Locations.hithaeglirFoothills
    getLocationThreat foothills `shouldReturn` 0
    run $ DrawCards $ Drawing pid 1
    run $ DrawCards $ Drawing pid 1
    getLocationThreat foothills `shouldReturn` 2

  it "draws 2 cards in the resource phase while Hithaeglir Stream is active" . trapTest $ \pid -> do
    _ <- active Locations.hithaeglirStream
    drawn <- handSize pid
    run $ BeginPhase ResourcePhase
    handSize pid `shouldReturn` drawn + 2

  it "draws no card in the resource phase while Plains of Enedwaith is active" . trapTest $ \pid -> do
    _ <- active Locations.plainsOfEnedwaith
    drawn <- handSize pid
    run $ BeginPhase ResourcePhase
    handSize pid `shouldReturn` drawn

  it "gives Dunland enemies +1 attack and defense with Munuv Dûv Ravine" . trapTest $ \pid -> do
    warrior <- engaged pid Enemies.boarClanWarrior
    _ <- staged Locations.munuvDuvRavine
    getEnemyAttack warrior `shouldReturn` 4
    getEnemyDefense warrior `shouldReturn` 2

  it "discards each ally in hand with Frenzied Attack, surging if there was none" . trapTest $ \pid -> do
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Treacheries.frenziedAttack, Locations.plainsOfEnedwaith, Locations.plainsOfEnedwaith]
    run RevealEncounterCard
    resolveRevealed
    player <- getPlayer pid
    map toCardCode player.hand `shouldSatisfy` notElem (toCardCode Allies.guardOfTheCitadel)
    -- an ally was discarded, so it did not surge
    placementOf Locations.plainsOfEnedwaith `shouldReturn` Nothing

  it "surges with Frenzied Attack when no ally was discarded" . trapTest $ \pid -> do
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ [] & playersL . ix pid . handL .~ []
    _ <- setEncounterDeck [Treacheries.frenziedAttack, Locations.plainsOfEnedwaith]
    run RevealEncounterCard
    resolveRevealed
    placementOf Locations.plainsOfEnedwaith `shouldReturn` Just StagingArea

  it "attaches Dunlending Ambush to the active location, engaging a Dunland enemy as it is explored" . trapTest $ \pid -> do
    road <- active Locations.oldSouthRoad
    _ <- setEncounterDeck [Treacheries.dunlendingAmbush, Treacheries.dunlendingAmbush, Locations.plainsOfEnedwaith, Enemies.boarClanWarrior]
    run RevealEncounterCard
    resolveRevealed
    -- the second copy surged in, and found the first already there
    ambushes <- getsGame (toList . (.entities.treacheries))
    map (.placement) ambushes `shouldBe` [AttachedTo (toTarget road)]
    run $ PlaceTokens GameSource (toTarget road) Progress 4
    pickFirst pid
    placementOf Enemies.boarClanWarrior `shouldReturn` Just (EngagedWith pid)

  it "springs A Well Laid Trap: Munuv Dûv Ravine, Items discarded, one ally kept, a Boar Clan enemy" . trapTest $ \pid -> do
    flipFirstQuest pid
    pickFirst pid
    eowyn <- toTarget <$> getHeroWithCode "01007"
    stone <- putAttachmentOn pid Attachments.celebriansStone eowyn
    guard1 <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . CompleteQuest . toId =<< getActiveQuest
    resolveWhenRevealed pid
    chooseTarget pid guard1
    pickFirst pid
    -- the enemies engaged answer the draw, in the order the lead picks
    resolveForced pid
    placementOf Locations.munuvDuvRavine `shouldReturn` Just ActiveLocation
    placementOf Locations.oldSouthRoad `shouldReturn` Just StagingArea
    attachments <- getsGame (map toTarget . toList . (.entities.attachments))
    attachments `shouldSatisfy` notElem stone
    allies <- getsGame (map toTarget . toList . (.entities.allies))
    allies `shouldBe` [guard1]
    player <- getPlayer pid
    player.discard `shouldBe` []
    g <- getGame
    [e | e <- toList g.entities.enemies, e.placement == EngagedWith pid] `shouldSatisfy` ((== 2) . length)
    -- it advances at the end of the combat phase
    run $ CheckWindows [Window When (PhaseEnds CombatPhase)]
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.noWayOut

  it "puts Chief Turch into play with No Way Out, Time 5 a player, a counter off as an enemy engages" . trapTest $ \pid -> do
    turch <- genEncounterCard Enemies.chiefTurchTheDunlandTrap
    run $ SetAside turch
    q <- putQuestStage Quests.noWayOut
    run $ Flip q
    resolveWhenRevealed pid
    getTimeX q `shouldReturn` 5
    placementOf Enemies.chiefTurchTheDunlandTrap `shouldReturn` Just (EngagedWith pid)
    -- his own engagement took 1 off
    getTimeCounters q `shouldReturn` 4
    _ <- engaged pid Enemies.boarClanWarrior
    getTimeCounters q `shouldReturn` 3
    -- he cannot leave play, not even destroyed
    eid <- enemyOf Enemies.chiefTurchTheDunlandTrap
    run $ Discard (toTarget eid)
    run $ DealDamage GameSource (toTarget eid) 10
    placementOf Enemies.chiefTurchTheDunlandTrap `shouldReturn` Just (EngagedWith pid)

  it "has Chief Turch engage the first player as the token passes" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
    run $ SetFirstPlayer one
    turch <- engaged one Enemies.chiefTurchTheDunlandTrap
    run $ SetFirstPlayer two
    placementOf Enemies.chiefTurchTheDunlandTrap `shouldReturn` Just (EngagedWith two)
    -- the other player cannot engage him
    run $ PlaceEnemy turch StagingArea
    run $ OptionalEngagement one
    offered <- maybe False (any \case TargetLabel t _ -> t == toTarget turch; _ -> False) . fmap questionChoices <$> getQuestion one
    offered `shouldBe` False

  it "has Chief Turch engage the first player whoever a card would have engage him" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
    run $ SetFirstPlayer one
    run . AddToStaging =<< genEncounterCard Enemies.chiefTurchTheDunlandTrap
    turch <- enemyOf Enemies.chiefTurchTheDunlandTrap
    run $ EngageEnemy two turch
    placementOf Enemies.chiefTurchTheDunlandTrap `shouldReturn` Just (EngagedWith one)

  it "is lost as a hero is destroyed at No Way Out" . trapTest $ \_ -> do
    _ <- putQuestStage Quests.noWayOut
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ DealDamage GameSource hero 10
    outcome `shouldReturn` Just Lost

  it "is won once No Way Out's last counter is removed and no hero falls to the attacks" . trapTest $ \pid -> do
    q <- putQuestStage Quests.noWayOut
    run $ PlaceTokens GameSource q TimeCounter 1
    -- the engagement checks engage it: its attack comes first
    run . AddToStaging =<< genEncounterCard Enemies.boarClanWarrior
    -- a shadow card with no effect
    _ <- setEncounterDeck [Locations.hithaeglirFoothills]
    run $ RaiseThreat pid 30
    run $ RemoveTokens GameSource q TimeCounter 1
    placementOf Enemies.boarClanWarrior `shouldReturn` Just (EngagedWith pid)
    outcome `shouldReturn` Nothing
    takeUndefended pid "01012"
    outcome `shouldReturn` Just Won

  it "is lost if a hero falls to No Way Out's last attacks" . trapTest $ \pid -> do
    q <- putQuestStage Quests.noWayOut
    run $ PlaceTokens GameSource q TimeCounter 1
    _ <- engaged pid Enemies.boarClanWarrior
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource beravor Damage 3
    _ <- setEncounterDeck [Locations.plainsOfEnedwaith]
    run $ RemoveTokens GameSource q TimeCounter 1
    takeUndefended pid "01012"
    outcome `shouldReturn` Just Lost

-- | Take the attack undefended, its damage on the hero.
takeUndefended :: PlayerId -> CardCode -> GameT ()
takeUndefended pid hero = do
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid
  chooseTarget pid . toTarget =<< getHeroWithCode hero
  passActions pid
