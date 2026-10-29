module LordOfTheRings.AgainstTheShadow.TheMorgulValeSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theMorgulVale)
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (towerProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (RoundEnds))
import TestImport

valeTest :: (PlayerId -> GameT ()) -> IO ()
valeTest = gameTestIn theMorgulVale testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [o.placement | o <- toList g.entities.objectives, toCardCode o == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

-- | To the Tower, in the staging area.
tower :: GameT ObjectiveId
tower = do
  run . AddToStaging =<< genEncounterCard Objectives.toTheTower
  getsGame (fromJustNote "no tower" . fmap toId . headMay . toList . (.entities.objectives))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Take the attack undefended, its damage on the hero.
takeUndefended :: PlayerId -> CardCode -> GameT ()
takeUndefended pid hero = do
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid
  chooseTarget pid . toTarget =<< getHeroWithCode hero
  passActions pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

spec :: Spec
spec = describe "The Morgul Vale" do
  it "is a scenario the game can be dealt" . valeTest $ \_ ->
    isJust (lookupScenario "the-morgul-vale") `shouldBe` True

  it "stages To the Tower, Murzag and a Morgul Vale, sets the others aside and removes Faramir" . valeTest $ \pid -> do
    flipFirstQuest pid
    for_ [Enemies.murzag, Locations.morgulVale] \c -> placementOf c `shouldReturn` Just StagingArea
    placementOf Objectives.toTheTower `shouldReturn` Just StagingArea
    deck <- map toCardCode <$> getEncounterDeck
    for_ [Enemies.lordAlcaron, Enemies.nazgulOfMinasMorgul, Locations.theWhiteBridge, Objectives.toTheTower] \c ->
      deck `shouldSatisfy` notElem (toCardCode c)
    player <- getPlayer pid
    map toCardCode (player.deck <> player.hand)
      `shouldSatisfy` notElem (toCardCode Allies.faramir)

  it "is lost once To the Tower has 10 progress" . valeTest $ \_ -> do
    oid <- tower
    run $ PlaceTokens GameSource (toTarget oid) Progress 9
    outcome `shouldReturn` Nothing
    run $ PlaceTokens GameSource (toTarget oid) Progress 1
    outcome `shouldReturn` Just Lost

  it "keeps Murzag in play unless destroyed, and advances to The Betrayal as he is" . valeTest $ \pid -> do
    flipFirstQuest pid
    murzag <- enemyOf Enemies.murzag
    run $ Discard (toTarget murzag)
    run $ ShuffleIntoEncounterDeck (toTarget murzag)
    placementOf Enemies.murzag `shouldReturn` Just StagingArea
    run $ DealDamage GameSource (toTarget murzag) 7
    resolveWhenRevealed pid
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.theBetrayal
    placementOf Enemies.lordAlcaron `shouldReturn` Just StagingArea

  it "deals the damage meant for a Captain to Morgul Bodyguard" . valeTest $ \_ -> do
    murzag <- staged Enemies.murzag
    guard' <- staged Enemies.morgulBodyguard
    run $ DealDamage GameSource (toTarget murzag) 3
    getDamage (toTarget murzag) `shouldReturn` 0
    getDamage (toTarget guard') `shouldReturn` 3

  it "reduces any damage dealt to Nazgûl of Minas Morgul to 1, and wins as it is defeated" . valeTest $ \_ -> do
    _ <- putQuestStage Quests.theMorgulRider
    nazgul <- staged Enemies.nazgulOfMinasMorgul
    run $ DealDamage GameSource (toTarget nazgul) 6
    getDamage (toTarget nazgul) `shouldReturn` 1
    replicateM_ 4 $ run $ DealDamage GameSource (toTarget nazgul) 3
    outcome `shouldReturn` Just Won

  it "gives The White Bridge as much threat and as many quest points as To the Tower has progress" . valeTest $ \_ -> do
    oid <- tower
    run . AddToStaging =<< genEncounterCard Locations.theWhiteBridge
    bridge <- getsGame (fromJustNote "no bridge" . fmap toId . headMay . toList . (.entities.locations))
    run $ PlaceTokens GameSource (toTarget oid) Progress 4
    getLocationThreat bridge `shouldReturn` 4

  it "deals Morgul Sorcerer a shadow card as progress is placed on To the Tower" . valeTest $ \_ -> do
    oid <- tower
    sorcerer <- staged Enemies.morgulSorcerer
    _ <- setEncounterDeck [Enemies.morgulTracker]
    run $ PlaceTokens GameSource (toTarget oid) Progress 2
    length . (.shadowCards) <$> getEnemy sorcerer `shouldReturn` 1

  it "places progress on To the Tower as Morgul Tracker is optionally engaged" . valeTest $ \pid -> do
    _ <- tower
    tracker <- staged Enemies.morgulTracker
    run $ OptionalEngagement pid
    chooseTarget pid tracker
    towerProgress `shouldReturn` 1

  it "returns an engaged enemy to the staging area at the end of the round with Morgul Vale" . valeTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.morgulVale
    tracker <- staged Enemies.morgulTracker
    run $ EngageEnemy pid tracker
    run $ CheckWindows [Window When RoundEnds]
    placementOf Enemies.morgulTracker `shouldReturn` Just StagingArea

  it "places progress for Terror Drives Them and surges" . valeTest $ \_ -> do
    _ <- tower
    reveal [Treacheries.terrorDrivesThem, Treacheries.terrorDrivesThem, Locations.morgulRoad]
    towerProgress `shouldReturn` 2
    placementOf Locations.morgulRoad `shouldReturn` Just StagingArea

  it "raises threat by the progress on To the Tower with The Dead City Looms" . valeTest $ \pid -> do
    oid <- tower
    run $ PlaceTokens GameSource (toTarget oid) Progress 3
    threat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.theDeadCityLooms]
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3

  it "has Murzag reveal the top card after he attacks, engaging an enemy with the defender" . valeTest $ \pid -> do
    murzag <- staged Enemies.murzag
    run $ EngageEnemy pid murzag
    _ <- setEncounterDeck [Enemies.morgulSorcerer]
    run $ EnemyAttack pid murzag
    takeUndefended pid "01012"
    placementOf Enemies.morgulSorcerer `shouldReturn` Just (EngagedWith pid)

  it "has Lord Alcaron attack again when his shadow card has no effect" . valeTest $ \pid -> do
    _ <- tower
    alcaron <- staged Enemies.lordAlcaron
    run $ EngageEnemy pid alcaron
    -- Murzag has no shadow effect, Morgul Sorcerer has one
    _ <- setEncounterDeck [Enemies.murzag, Enemies.morgulSorcerer]
    run $ DealShadowCard alcaron
    run $ EnemyAttack pid alcaron
    takeUndefended pid "01007"
    -- the second attack, after the first
    takeUndefended pid "01002"
    -- each attack places 1 progress
    towerProgress `shouldReturn` 2
    getQuestion pid >>= (`shouldSatisfy` maybe True (\case QuestionLabel l _ _ _ -> not ("$label.declareDefender" `isPrefixOf` l); _ -> True))

  it "places progress with Morgul Vale's shadow once its attack destroys a character" . valeTest $ \pid -> do
    _ <- tower
    tracker <- staged Enemies.morgulTracker
    run $ EngageEnemy pid tracker
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Damage 2
    _ <- setEncounterDeck [Locations.morgulVale]
    run $ DealShadowCard tracker
    run $ EnemyAttack pid tracker
    takeUndefended pid "01007"
    towerProgress `shouldReturn` 1

  -- "If this attack destroys a character" counts only a character the attack's own damage
  -- destroys: see 'attackDestroyed'
  it "places progress with Morgul Vale's shadow once its attack's damage destroys the defender" . valeTest $ \pid -> do
    _ <- tower
    tracker <- staged Enemies.morgulTracker
    run $ EngageEnemy pid tracker
    scout <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ PlaceTokens GameSource scout Damage 1
    _ <- setEncounterDeck [Locations.morgulVale]
    run $ DealShadowCard tracker
    run $ EnemyAttack pid tracker
    passActions pid
    chooseTarget pid scout
    passActions pid
    towerProgress `shouldReturn` 1

  it "places no progress with Morgul Vale's shadow for a defender discarded some other way during the attack" . valeTest $ \pid -> do
    _ <- tower
    tracker <- staged Enemies.morgulTracker
    run $ EngageEnemy pid tracker
    scout <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Locations.morgulVale]
    run $ DealShadowCard tracker
    run $ EnemyAttack pid tracker
    passActions pid
    chooseTarget pid scout
    -- the defender leaves play before the damage, which goes to a hero
    runOnly $ Discard scout
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01002"
    passActions pid
    towerProgress `shouldReturn` 0

  -- the FAQ's Foe-hammer ruling: destroying takes the attack's damage, from its strength; a
  -- shadow effect's damage is the shadow card's effect, not the attack's
  it "places no progress with Morgul Vale's shadow for a defender another shadow card's damage destroys" . valeTest $ \pid -> do
    _ <- tower
    tracker <- staged Enemies.morgulTracker
    run $ EngageEnemy pid tracker
    scout <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ PlaceTokens GameSource scout Damage 1
    _ <- setEncounterDeck [Locations.morgulVale, Treacheries.festeringWounds]
    run $ DealShadowCard tracker
    run $ DealShadowCard tracker
    run $ EnemyAttack pid tracker
    passActions pid
    chooseTarget pid scout
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01002"
    passActions pid
    getsGame (any ((== scout) . toTarget) . toList . (.entities.allies)) `shouldReturn` False
    towerProgress `shouldReturn` 0

  it "lets the defending player treat the attack as undefended with Morgul Sorcerer's shadow" . valeTest $ \pid -> do
    _ <- tower
    tracker <- staged Enemies.morgulTracker
    run $ EngageEnemy pid tracker
    _ <- setEncounterDeck [Enemies.morgulSorcerer]
    run $ DealShadowCard tracker
    run $ EnemyAttack pid tracker
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01012"
    passActions pid
    chooseLabel "$label.morgulSorcerer.undefended" pid
    passActions pid
    theodred <- toTarget <$> getHeroWithCode "01002"
    chooseTarget pid theodred
    passActions pid
    getDamage theodred `shouldReturn` 1
    towerProgress `shouldReturn` 0

  it "has the first player pick which Morgul Bodyguard takes a Captain's damage" . valeTest $ \pid -> do
    murzag <- staged Enemies.murzag
    run . AddToStaging =<< genEncounterCard Enemies.morgulBodyguard
    run . AddToStaging =<< genEncounterCard Enemies.morgulBodyguard
    guards <- getsGame (map toId . filter ((== toCardCode Enemies.morgulBodyguard) . toCardCode) . toList . (.entities.enemies))
    run $ DealDamage GameSource (toTarget murzag) 2
    case guards of
      [_, picked] -> do
        chooseTarget pid picked
        getDamage (toTarget picked) `shouldReturn` 2
        getDamage (toTarget murzag) `shouldReturn` 0
      _ -> error "two bodyguards"
