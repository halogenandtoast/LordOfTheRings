module LordOfTheRings.HeirsOfNumenor.PerilInPelargirSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer), getArcheryTotal)
import LordOfTheRings.Modifier (ModifierType (CannotBeOptionallyEngagedBy))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, perilInPelargir)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PhaseBegins))
import TestImport

pelargirTest :: (PlayerId -> GameT ()) -> IO ()
pelargirTest = gameTestIn perilInPelargir testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [t.placement | t <- toList g.entities.treacheries, toCardCode t == code]
    <> [o.placement | o <- toList g.entities.objectives, toCardCode o == code]

enemiesOf :: CardDef -> GameT [EnemyId]
enemiesOf def = getsGame (map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = fromJustNote "no such enemy" . headMay <$> enemiesOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Alcaron's Scroll, attached to the hero.
scrollOn :: Target -> GameT ()
scrollOn host = run . (`PutObjectiveIntoPlayAttached` host) =<< genEncounterCard Objectives.alcaronsScroll

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "Peril in Pelargir" do
  it "is a scenario the game can be dealt" . pelargirTest $ \_ ->
    isJust (lookupScenario "peril-in-pelargir") `shouldBe` True

  it "makes The Leaping Fish active, gives a hero the scroll and stages a Harbor Thug at setup" . pelargirTest $ \pid -> do
    flipFirstQuest pid
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    placementOf Locations.theLeapingFish `shouldReturn` Just ActiveLocation
    placementOf Objectives.alcaronsScroll `shouldReturn` Just (AttachedTo beravor)
    thugs <- enemiesOf Enemies.harborThug
    length thugs `shouldBe` 1
    placementOf Enemies.harborThug `shouldReturn` Just StagingArea
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` notElem (toCardCode Locations.theLeapingFish))

  it "moves the scroll to the enemy whose attack damages its hero, and back to the staging area when that enemy leaves" . pelargirTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    scrollOn beravor
    thug <- staged Enemies.harborThug
    run $ DealDamage GameSource beravor 1
    placementOf Objectives.alcaronsScroll `shouldReturn` Just (AttachedTo beravor)
    run $ EngageEnemy pid thug
    run $ DealDamage (EnemyAttackSource thug) beravor 1
    placementOf Objectives.alcaronsScroll `shouldReturn` Just (AttachedTo $ toTarget thug)
    run $ Discard (toTarget thug)
    placementOf Objectives.alcaronsScroll `shouldReturn` Just StagingArea

  it "is attached to a hero from the staging area by exhausting it" . pelargirTest $ \pid -> do
    beravor <- getHeroWithCode "01012"
    scrollOn (toTarget beravor)
    oid <- getsGame (fromJustNote "no scroll" . fmap toId . headMay . toList . (.entities.objectives))
    run $ PlaceObjective oid StagingArea
    run $ ActionWindow pid
    chooseAbilityOf pid (ObjectiveTarget oid)
    chooseTarget pid beravor
    placementOf Objectives.alcaronsScroll `shouldReturn` Just (AttachedTo $ toTarget beravor)
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "engages Harbor Thug with the player holding the scroll when their threat rises" . pelargirTest $ \pid -> do
    scrollOn . toTarget =<< getHeroWithCode "01012"
    _ <- staged Enemies.harborThug
    run $ RaiseThreat pid 1
    placementOf Enemies.harborThug `shouldReturn` Just (EngagedWith pid)

  it "sends the attack's damage to the scroll's hero with Harbor Thug's shadow" . pelargirTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    scrollOn beravor
    thug <- staged Enemies.harborThug
    card <- genEncounterCard Enemies.harborThug
    run $ AttachShadowCard thug card
    run $ ResolveShadowCard card.id thug pid []
    run $ UndefendedDamage pid thug
    getDamage beravor `shouldReturn` 3
    getDamage eowyn `shouldReturn` 0

  it "attaches the scroll to the enemy with the highest engagement cost at Fighting in the Streets" . pelargirTest $ \pid -> do
    scrollOn . toTarget =<< getHeroWithCode "01012"
    _ <- staged Enemies.zealousTraitor
    assassin <- staged Enemies.umbarAssassin
    quest <- putQuestStage Quests.fightingInTheStreets
    run $ Flip quest
    resolveWhenRevealed pid
    placementOf Objectives.alcaronsScroll `shouldReturn` Just (AttachedTo $ toTarget assassin)

  it "cannot advance from Fighting in the Streets until the scroll is back on a hero" . pelargirTest $ \_ -> do
    thug <- staged Enemies.harborThug
    scrollOn (toTarget thug)
    quest <- putQuestStage Quests.fightingInTheStreets
    run $ PlaceTokens GameSource quest Progress 13
    getsGame (.outcome) `shouldReturn` Nothing
    oid <- getsGame (fromJustNote "no scroll" . fmap toId . headMay . toList . (.entities.objectives))
    beravor <- getHeroWithCode "01012"
    run $ ClaimObjective oid (toTarget beravor)
    -- with no stage left behind it, advancing wins
    getsGame (.outcome) `shouldReturn` Just Won

  it "has the first enemy revealed each round attack the scroll's holder at Escape to the Quays" . pelargirTest $ \pid -> do
    scrollOn . toTarget =<< getHeroWithCode "01012"
    _ <- putQuestStage Quests.escapeToTheQuays
    reveal [Enemies.zealousTraitor]
    passActions pid
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)
    placementOf Enemies.zealousTraitor `shouldReturn` Just StagingArea

  it "is won at Escape to the Quays only with the scroll on a hero" . pelargirTest $ \_ -> do
    thug <- staged Enemies.harborThug
    scrollOn (toTarget thug)
    quest <- putQuestStage Quests.escapeToTheQuays
    getModifiers GameTarget >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))
    run $ PlaceTokens GameSource quest Progress 15
    getsGame (.outcome) `shouldReturn` Nothing
    run $ Discard (toTarget thug)
    oid <- getsGame (fromJustNote "no scroll" . fmap toId . headMay . toList . (.entities.objectives))
    run . ClaimObjective oid . toTarget =<< getHeroWithCode "01012"
    getsGame (.outcome) `shouldReturn` Just Won

  it "discards 2 more cards for each Collateral Damage in the discard pile, raising threat by 2 for each location" . pelargirTest $ \pid -> do
    run . AddToEncounterDiscard . pure =<< genEncounterCard Treacheries.collateralDamage
    threat <- threatOf pid
    reveal [Treacheries.collateralDamage, Locations.cityStreet, Enemies.pickpocket, Locations.marketSquare, Enemies.zealousTraitor, Enemies.harborThug]
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.harborThug]
    threatOf pid `shouldReturn` threat + 4

  it "raises each player's threat by 1 for each location discarded while Harbor Storehouse is in play" . pelargirTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.harborStorehouse
    _ <- setEncounterDeck [Locations.cityStreet, Enemies.zealousTraitor]
    threat <- threatOf pid
    run $ DiscardTopOfEncounterDeck GameSource GameTarget 2
    threatOf pid `shouldReturn` threat + 1

  it "adds the enemies The Leaping Fish discards to the staging area while the scroll is on a hero" . pelargirTest $ \pid -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.theLeapingFish
    scrollOn . toTarget =<< getHeroWithCode "01012"
    _ <- setEncounterDeck [Enemies.zealousTraitor, Locations.cityStreet]
    run $ CheckWindows [Window After (PhaseBegins QuestPhase)]
    passActions pid
    placementOf Enemies.zealousTraitor `shouldReturn` Just StagingArea
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.cityStreet]

  it "has Zealous Traitor deal 2 damage to each ally of a player it engages unbidden" . pelargirTest $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.faramir
    traitor <- staged Enemies.zealousTraitor
    run $ EngageEnemy pid traitor
    getDamage ally `shouldReturn` 2

  it "has Zealous Traitor deal 1 damage to each ally of a player who engages it" . pelargirTest $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.faramir
    traitor <- staged Enemies.zealousTraitor
    run $ OptionalEngagement pid
    chooseTarget pid traitor
    getDamage ally `shouldReturn` 1

  it "counts Umbar Assassin's archery in the staging area" . pelargirTest $ \_ -> do
    _ <- staged Enemies.umbarAssassin
    getArcheryTotal `shouldReturn` 2

  it "returns engaged Brigands to the staging area with Lurking in Shadows" . pelargirTest $ \pid -> do
    traitor <- staged Enemies.zealousTraitor
    run $ EngageEnemy pid traitor
    reveal [Treacheries.lurkingInShadows]
    placementOf Enemies.zealousTraitor `shouldReturn` Just StagingArea

  it "attaches Local Trouble to the hero with the highest threat cost, raising threat as it exhausts" . pelargirTest $ \pid -> do
    reveal [Treacheries.localTrouble]
    -- Beravor costs 10, Éowyn 9 and Théodred 8
    beravor <- toTarget <$> getHeroWithCode "01012"
    placementOf Treacheries.localTrouble `shouldReturn` Just (AttachedTo beravor)
    threat <- threatOf pid
    run $ Exhaust beravor
    threatOf pid `shouldReturn` threat + 1

  it "raises threat as the Local Trouble hero is readied by a card effect" . pelargirTest $ \pid -> do
    reveal [Treacheries.localTrouble]
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ Exhaust beravor
    threat <- threatOf pid
    run $ Ready beravor
    threatOf pid `shouldReturn` threat + 1
    -- readying a hero already ready is no readying
    run $ Ready beravor
    threatOf pid `shouldReturn` threat + 1
