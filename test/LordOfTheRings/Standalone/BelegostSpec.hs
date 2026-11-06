module LordOfTheRings.Standalone.BelegostSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Attachment (attachmentHosts)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getEnemyAttack, getEnemyDefense, getEngagementCost)
import LordOfTheRings.Matcher (getEnemy, select, AttachMatcher (AttachToEnemy), EnemyMatcher (AnyEnemy, EnemyEngagedWith), PlayerMatcher (PlayerWithId))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import TestImport

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "missing enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies)

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  lid <- getsGame $ toId . fromJustNote "missing location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations)
  run $ PlaceLocation lid ActiveLocation
  pure lid

claimLoot :: PlayerId -> CardDef -> GameT ()
claimLoot pid def = do
  hero <- heroOf pid "01012"
  run . (`PutObjectiveIntoPlayAttached` hero) =<< genEncounterCard def

stats :: EnemyId -> GameT [Int]
stats eid = sequence [getEnemyThreat eid, getEnemyAttack eid, getEnemyDefense eid]

spec :: Spec
spec = describe "Belegost enemies" do
  describe "Lurker of the Depths" do
    it "is considered engaged with every player while remaining in staging" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      for_ [one, two] \pid -> do
        select (EnemyEngagedWith $ PlayerWithId pid) >>= (`shouldSatisfy` elem eid)
        ownAttackers pid eid >>= (`shouldSatisfy` notNull)
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "rejects both forced engagement and direct engaged placement" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      run $ EngageEnemy pid eid
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      run $ PlaceEnemy eid (EngagedWith pid)
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "takes at most four damage across separate hits each round and can be defeated" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      getDamage (EnemyTarget eid) `shouldReturn` 4
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 8
      getDamage (EnemyTarget eid) `shouldReturn` 8
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 8
      run $ CheckDefeated GameSource (EnemyTarget eid)
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))
        `shouldReturn` [toCardCode Enemies.lurkerOfTheDepths]

    it "prohibits attachments without preventing removal from play" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)
      attachmentHosts (AttachToEnemy AnyEnemy) >>= (`shouldSatisfy` notElem (EnemyTarget eid))
      run $ Discard GameSource (EnemyTarget eid)
      getsGame (any ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` False

  describe "Nameless Cave Dweller" do
    it "has one threat, attack and defense without an active Discover location" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      stats eid `shouldReturn` [1, 1, 1]
      _ <- activeLocation Locations.oldForestRoad
      stats eid `shouldReturn` [1, 1, 1]
      getModifiers eid >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)
      attachmentHosts (AttachToEnemy AnyEnemy) >>= (`shouldSatisfy` notElem (EnemyTarget eid))

    it "reads the active location's modified Discover value dynamically" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      lid <- activeLocation Locations.hauntedHall
      stats eid `shouldReturn` [5, 5, 5]
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [DiscoverValueModifier 2] UntilEndOfPhase
      stats eid `shouldReturn` [7, 7, 7]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfPhase
      stats eid `shouldReturn` [1, 1, 1]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]
      run $ PlaceLocation lid StagingArea
      stats eid `shouldReturn` [1, 1, 1]

    it "loses its variable stats while its text is blank" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      _ <- activeLocation Locations.hauntedHall
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfPhase
      stats eid `shouldReturn` [0, 0, 0]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]

    for_ [False, True] \hasLoot ->
      it ("gives its shadow +" <> if hasLoot then "2 when the defender controls Loot" else "1 without Loot") . gameTest $ \pid -> do
        when hasLoot $ claimLoot pid Objectives.cryptKey
        eid <- stageEnemy Enemies.namelessCaveDweller
        cid <- getsGame $ (.cardId) . toAttrs . fromJustNote "missing dweller" . find ((== eid) . toId) . toList . (.entities.enemies)
        run $ ResolveShadowEffect cid eid pid []
        getEnemyAttack eid `shouldReturn` if hasLoot then 3 else 2
        run $ EndEffects (UntilEnemyAttackEnds eid)
        getEnemyAttack eid `shouldReturn` 1

    it "does not count another player's Loot for its shadow bonus" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.cryptKey
      eid <- stageEnemy Enemies.namelessCaveDweller
      cid <- (.cardId) . toAttrs <$> getEnemy eid
      run $ ResolveShadowEffect cid eid one []
      getEnemyAttack eid `shouldReturn` 2

  describe "Old Stone Troll" do
    it "reduces engagement cost for each claimed Loot, excluding guarded Loot" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      getEngagementCost eid `shouldReturn` 50
      claimLoot pid Objectives.cryptKey
      claimLoot pid Objectives.oldLantern
      getEngagementCost eid `shouldReturn` 30
      run . (`PutObjectiveIntoPlayAttached` (EnemyTarget eid)) =<< genEncounterCard Objectives.scathasTeeth
      getEngagementCost eid `shouldReturn` 30
      getModifiers eid >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)

    it "allows only its engaged player's attackers, while permitting sentinel defense" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      other <- heroOf two "01012"
      run $ CreateEffect $ Effect GameSource other [GainKeyword Ranged, GainKeyword Sentinel] UntilEndOfRound
      run $ EngageEnemy one eid
      ownAttackers one eid >>= (`shouldSatisfy` notNull)
      ownAttackers two eid `shouldReturn` []
      mayAttack one eid >>= (`shouldSatisfy` notElem other)
      canAttack eid other `shouldReturn` False
      canDefend eid other `shouldReturn` True
      getModifiers eid >>= (`shouldSatisfy` notElem OnlyEngagedPlayerFights)
      run $ DeclareRangedAttackers one eid [two]
      getQuestion two `shouldReturn` Nothing

    it "does not allow staging attacks before it engages a player" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      hero <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource hero [CanAttackStagingArea] UntilEndOfRound
      ownAttackers pid eid `shouldReturn` []
