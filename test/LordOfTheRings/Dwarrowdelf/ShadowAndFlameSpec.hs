module LordOfTheRings.Dwarrowdelf.ShadowAndFlameSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyEngagedWith), PlayerMatcher (PlayerWithId), select)
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (shadowAndFlame)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

flameTest :: (PlayerId -> GameT ()) -> IO ()
flameTest = gameTestIn shadowAndFlame testDeck

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

-- | Durin's Bane in the staging area, each player's threat raised to 20 first.
bane :: GameT EnemyId
bane = do
  players <- getsGame (.playerOrder)
  for_ players \p -> run $ RaiseThreat p 20
  run . AddToStaging =<< genEncounterCard Enemies.durinsBane
  enemyOf Enemies.durinsBane

shadowCount :: EnemyId -> GameT Int
shadowCount eid = getsGame (maybe 0 (length . (.shadowCards)) . lookup eid . (.entities.enemies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

stageCode :: GameT (Maybe CardCode)
stageCode = getsGame (fmap toCardCode . headMay . toList . (.entities.quests))

-- | Pass the action windows of every player at the table, as many as are open.
passAll :: [PlayerId] -> GameT ()
passAll pids = do
  was <- getsGame (.questionVersion)
  traverse_ passActions pids
  now <- getsGame (.questionVersion)
  when (now /= was) $ passAll pids

-- | The player takes the attack undefended, the damage on the first hero offered.
takeUndefended :: [PlayerId] -> PlayerId -> GameT ()
takeUndefended table pid = do
  passAll table
  chooseLabel "$label.takeAttackUndefended" pid
  passAll table
  chooseOptionMatching "take the damage" pid \case
    TargetLabel {} -> True
    _ -> False
  passAll table

-- | Whether Counter-spell is still attached to Durin's Bane.
counterSpellOn :: EnemyId -> GameT Bool
counterSpellOn eid =
  getsGame (any (\t -> toCardCode t == toCardCode Treacheries.counterSpell && t.placement == AttachedTo (toTarget eid)) . toList . (.entities.treacheries))

-- | Counter-spell on Durin's Bane, a card in hand and the encounter deck's top card given,
-- then Lórien's Wealth played.
counterSpellPlay :: PlayerId -> CardDef -> GameT (EnemyId, Card)
counterSpellPlay pid top = do
  eid <- bane
  reveal [Treacheries.counterSpell]
  _ <- putCardIntoHand pid Allies.guardOfTheCitadel
  _ <- setEncounterDeck [top]
  (eid,) <$> playWith pid "01012" 3 Events.loriensWealth

spec :: Spec
spec = describe "Shadow and Flame" do
  it "starts with Durin's Bane in the staging area, Dark Pit set aside and each threat at 0" . flameTest $ \pid -> do
    flipFirstQuest pid
    eid <- enemyOf Enemies.durinsBane
    getsGame (fmap (.placement) . lookup eid . (.entities.enemies)) `shouldReturn` Just StagingArea
    setAside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    setAside `shouldSatisfy` elem (toCardCode Locations.darkPit)
    (.threat) <$> getPlayer pid `shouldReturn` 0

  it "is engaged with each player whose threat is 1 or more, and never leaves the staging area" . flameTest $ \pid -> do
    eid <- bane
    select (EnemyEngagedWith $ PlayerWithId pid) `shouldReturn` [eid]
    run $ LowerThreat pid 100
    select (EnemyEngagedWith $ PlayerWithId pid) `shouldReturn` []
    run $ EngageEnemy pid eid
    getsGame (fmap (.placement) . lookup eid . (.entities.enemies)) `shouldReturn` Just StagingArea

  it "is not destroyed by damage past its hit points" . flameTest $ \_ -> do
    eid <- bane
    run $ DealDamage GameSource (toTarget eid) 30
    getDamage (toTarget eid) `shouldReturn` 30

  it "lets a player's characters attack it as engaged, not as in the staging area" . flameTest $ \pid -> do
    eid <- bane
    attackers <- mayAttack pid eid
    attackers `shouldSatisfy` notNull

  it "attacks each player in turn, dealt and discarding a new shadow card each attack" . twoPlayerTest testDeck $ \one two -> do
    eid <- bane
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run DealShadowCards
    shadowCount eid `shouldReturn` 0
    run $ EnemyAttacks one
    shadowCount eid `shouldReturn` 1
    takeUndefended [one, two] one
    shadowCount eid `shouldReturn` 0
    length <$> getEncounterDiscard `shouldReturn` 1
    run $ EnemyAttacks two
    shadowCount eid `shouldReturn` 1
    takeUndefended [one, two] two
    length <$> getEncounterDiscard `shouldReturn` 2

  it "carries Fiery Sword for +3 attack" . flameTest $ \_ -> do
    eid <- bane
    reveal [Treacheries.fierySword]
    getEnemyAttack eid `shouldReturn` 9

  it "heals Durin's Bane with Inner Shadow, unless a questing hero leaves the quest" . flameTest $ \pid -> do
    eid <- bane
    run $ PlaceTokens GameSource (toTarget eid) Damage 10
    reveal [Treacheries.innerShadow, Locations.oldForestRoad]
    getDamage (toTarget eid) `shouldReturn` 5
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ CommitToQuest beravor
    reveal [Treacheries.innerShadow, Locations.oldForestRoad]
    chooseTarget pid beravor
    getDamage (toTarget eid) `shouldReturn` 5
    heroCommitted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it "takes no progress on Second Deep while Durin's Bane is undamaged" . flameTest $ \_ -> do
    eid <- bane
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.secondDeep
    deep <- locationOf Locations.secondDeep
    run $ PlaceTokens GameSource (toTarget deep) Progress 2
    (.progress) <$> getLocation deep `shouldReturn` 0
    run $ PlaceTokens GameSource (toTarget eid) Damage 1
    run $ PlaceTokens GameSource (toTarget deep) Progress 2
    (.progress) <$> getLocation deep `shouldReturn` 2

  it "shuffles Ranging Goblin and the top of the discard pile back in after a character leaves play" . flameTest $ \pid -> do
    reveal [Enemies.rangingGoblin]
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.oldForestRoad
    _ <- setEncounterDeck []
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    guard' <- getsGame (toTarget . fromJustNote "no ally" . headMay . toList . (.entities.allies))
    run $ Discard guard'
    getsGame (any ((== toCardCode Enemies.rangingGoblin) . toCardCode) . toList . (.entities.enemies)) `shouldReturn` False
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` \cs -> sort cs == sort [toCardCode Enemies.rangingGoblin, toCardCode Locations.oldForestRoad])

  it "places 4 progress on The Rear Guard at the end of the quest phase once a hero committed" . flameTest $ \_ -> do
    _ <- bane
    _ <- putQuestStage Quests.theRearGuard
    run . CommitToQuest . toTarget =<< getHeroWithCode "01012"
    run $ EndPhase QuestPhase
    (.progress) <$> getActiveQuest `shouldReturn` 4

  it "places The Rear Guard's 4 progress for a hero that committed and has since left the quest" . flameTest $ \_ -> do
    _ <- bane
    _ <- putQuestStage Quests.theRearGuard
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ CommitToQuest beravor
    run $ RemoveFromQuest beravor
    run $ EndPhase QuestPhase
    (.progress) <$> getActiveQuest `shouldReturn` 4

  it "places no progress on The Rear Guard when no hero committed this round" . flameTest $ \_ -> do
    _ <- bane
    _ <- putQuestStage Quests.theRearGuard
    run $ EndPhase QuestPhase
    (.progress) <$> getActiveQuest `shouldReturn` 0

  it "advances from The Rear Guard once Durin's Bane has no hit points left, and it attacks the first player" . flameTest $ \pid -> do
    eid <- bane
    _ <- putQuestStage Quests.theRearGuard
    lastLord <- genEncounterCard Quests.lastLordOfMoria
    run . SetAside =<< genEncounterCard Locations.darkPit
    getGame >>= putGame . (scenarioL %~ overAttrs (questDeckL .~ [lastLord]))
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ DealDamage GameSource (toTarget eid) 27
    stageCode `shouldReturn` Just (toCardCode Quests.lastLordOfMoria)
    resolveWhenRevealed pid
    shadowCount eid `shouldReturn` 1
    takeUndefended [pid] pid
    _ <- locationOf Locations.darkPit
    pure ()

  it "cannot be won while Durin's Bane is in play, and is won as Dark Pit discards it" . flameTest $ \pid -> do
    eid <- bane
    _ <- putQuestStage Quests.lastLordOfMoria
    stage <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource stage Progress 3
    getsGame (.outcome) `shouldReturn` Nothing
    run $ PlaceTokens GameSource (toTarget eid) Damage 25
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.darkPit
    pit <- locationOf Locations.darkPit
    gandalfs <- replicateM 3 (PlayerCard <$> genPlayerCard Allies.gandalf (Just pid))
    getGame >>= putGame . (playersL . ix pid . deckL .~ gandalfs)
    run $ UseCardAbility pid (LocationSource pit) 1 []
    for_ ["01012", "01007"] \code -> chooseTarget pid . toTarget =<< getHeroWithCode code
    chooseLabel "$label.darkPit.done" pid
    getsGame (.outcome) `shouldReturn` Just Won

  it "has the defending player discard a card at random as Durin's Bane attacks with Many Thonged Whip" . flameTest $ \pid -> do
    _ <- bane
    reveal [Treacheries.manyThongedWhip]
    replicateM_ 2 $ putCardIntoHand pid Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ EnemyAttacks pid
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "cancels an event and discards its player's hand as Counter-spell discards a treachery" . flameTest $ \pid -> do
    (eid, card) <- counterSpellPlay pid Treacheries.innerFlame
    (.hand) <$> getPlayer pid `shouldReturn` []
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Treacheries.innerFlame))
    counterSpellOn eid `shouldReturn` False

  it "lets an event resolve as Counter-spell discards a card that is not a treachery" . flameTest $ \pid -> do
    (eid, _) <- counterSpellPlay pid Locations.oldForestRoad
    length . (.hand) <$> getPlayer pid `shouldReturn` 4
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.oldForestRoad]
    counterSpellOn eid `shouldReturn` True
