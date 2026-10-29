module LordOfTheRings.KhazadDum.MistyMountainsSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (EnemyAttrs (enemyShadowRevealed))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Modifier (ModifierType (CannotBeOptionallyEngagedBy))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyPlacement :: EnemyId -> GameT (Maybe Placement)
enemyPlacement eid = getsGame (fmap (.placement) . lookup eid . (.entities.enemies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | A Goblin Scout engaged with the player, dealt the shadow card.
engagedWithShadow :: PlayerId -> CardDef -> GameT EnemyId
engagedWithShadow pid shadow = do
  reveal [Enemies.goblinScout]
  eid <- enemyOf Enemies.goblinScout
  run $ EngageEnemy pid eid
  run . AttachShadowCard eid =<< genEncounterCard shadow
  pure eid

shadowCount :: EnemyId -> GameT Int
shadowCount eid = getsGame (maybe 0 (length . (.shadowCards)) . lookup eid . (.entities.enemies))

heroResources :: GameT [Int]
heroResources = getsGame (map (.resources) . toList . (.entities.heroes))

spec :: Spec
spec = describe "Misty Mountains" do
  it "has Black Uruks discard an attachment from a questing character" . gameTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    blade <- putAttachmentOn pid Attachments.bladeOfGondolin beravor
    plate <- putAttachmentOn pid Attachments.citadelPlate eowyn
    run $ CommitToQuest beravor
    reveal [Enemies.blackUruks]
    offersPlate <- maybe False (any \case TargetLabel t _ -> t == plate; _ -> False) . fmap questionChoices <$> getQuestion pid
    offersPlate `shouldBe` False
    chooseTarget pid blade
    getsGame (map toTarget . toList . (.entities.attachments)) `shouldReturn` [plate]

  it "has Black Uruks discard an encounter card attached to a questing character" . gameTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ CommitToQuest beravor
    _ <- setEncounterDeck [Treacheries.watchfulEyes]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid beravor
    eyes <- getsGame (map toTarget . toList . (.entities.treacheries))
    reveal [Enemies.blackUruks]
    asked <- isJust <$> getQuestion pid
    when asked $ for_ eyes (chooseTarget pid)
    getsGame (length . toList . (.entities.treacheries)) `shouldReturn` 0

  it "does not trigger a Watchful Eyes attached as the combat phase ends, as it was not in play then" . gameTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    _ <- setEncounterDeck [Treacheries.watchfulEyes]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid beravor
    run $ Exhaust beravor
    run $ Exhaust eowyn
    _ <- setEncounterDeck [Treacheries.watchfulEyes, Locations.kneesOfTheMountain, Locations.wargLair]
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    resolveRevealed
    asked <- isJust <$> getQuestion pid
    when asked $ chooseTarget pid eowyn
    resolveRevealed
    length <$> getEncounterDeck `shouldReturn` 2

  it "deals 2 more shadow cards with Black Uruks' shadow, only when undefended" . gameTest $ \pid -> do
    eid <- engagedWithShadow pid Enemies.blackUruks
    _ <- setEncounterDeck [Locations.kneesOfTheMountain, Locations.wargLair]
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ ResolveShadow eid pid [beravor]
    shadowCount eid `shouldReturn` 1
    eid' <- engagedWithShadow pid Enemies.blackUruks
    _ <- setEncounterDeck [Locations.kneesOfTheMountain, Locations.wargLair]
    run $ ResolveShadow eid' pid []
    resolveShadowsInOrder pid
    shadowCount eid' `shouldReturn` 3
    revealed <- getsGame (maybe [] (enemyShadowRevealed . toAttrs) . lookup eid' . (.entities.enemies))
    length revealed `shouldBe` 3

  it "returns Mountain Warg to staging after attacking with a shadow card with no effect" . gameTest $ \pid -> do
    reveal [Enemies.mountainWarg]
    eid <- enemyOf Enemies.mountainWarg
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Locations.kneesOfTheMountain
    run $ EnemyAttack pid eid
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid =<< (toTarget <$> getHeroWithCode "01012")
    passActions pid
    enemyPlacement eid `shouldReturn` Just StagingArea

  it "gives the attacker +1 with Mountain Warg's shadow, +2 at a Mountain" . gameTest $ \pid -> do
    eid <- engagedWithShadow pid Enemies.mountainWarg
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 2
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.turbulentWaters
    eid' <- engagedWithShadow pid Enemies.mountainWarg
    run $ ResolveShadow eid' pid []
    getEnemyAttack eid' `shouldReturn` 3

  it "has Bitter Wind discard 3 resources from each hero, 2 as a shadow" . gameTest $ \pid -> do
    heroes <- getsGame (map toTarget . toList . (.entities.heroes))
    for_ heroes \h -> run $ PlaceTokens GameSource h Resource 4
    reveal [Treacheries.bitterWind]
    heroResources `shouldReturn` [1, 1, 1]
    eid <- engagedWithShadow pid Treacheries.bitterWind
    run $ ResolveShadow eid pid []
    heroResources `shouldReturn` [0, 0, 0]

  it "gives an enemy +1 attack as it engages while Knees of the Mountain is in staging" . gameTest $ \pid -> do
    reveal [Locations.kneesOfTheMountain]
    reveal [Enemies.goblinScout]
    eid <- enemyOf Enemies.goblinScout
    run $ EngageEnemy pid eid
    knees <- locationOf Locations.kneesOfTheMountain
    whenM (offersAbilityOf pid knees) $ chooseAbilityOf pid knees
    getEnemyAttack eid `shouldReturn` 2

  it "stops optional engagement while Turbulent Waters is active" . gameTest $ \_ -> do
    reveal [Locations.turbulentWaters]
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (CannotBeOptionallyEngagedBy AnyPlayer))
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.turbulentWaters
    getModifiers GameTarget >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))

  it "has Warg Lair add Mountain Warg from the discard pile, and draw a card once explored" . gameTest $ \pid -> do
    warg <- genEncounterCard Enemies.mountainWarg
    run $ AddToEncounterDiscard [warg]
    reveal [Locations.wargLair]
    chooseTarget pid warg
    (enemyPlacement =<< enemyOf Enemies.mountainWarg) `shouldReturn` Just StagingArea
    lair <- locationOf Locations.wargLair
    run $ PlaceLocation lair ActiveLocation
    hand <- length . (.hand) <$> getPlayer pid
    run $ PlaceTokens GameSource (toTarget lair) Progress 3
    chooseAbilityOf pid lair
    length . (.hand) <$> getPlayer pid `shouldReturn` hand + 1
