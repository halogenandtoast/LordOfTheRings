module LordOfTheRings.Standalone.PrancingPonySpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getThreat)
import LordOfTheRings.Modifier (ModifierType (Blank, DefenseModifier))
import TestImport hiding (gameTest, twoPlayerTest)
import TestImport qualified as Test

gameTest :: (PlayerId -> GameT ()) -> IO ()
gameTest body = Test.gameTest $ \pid -> run (DoStep 2 Setup) >> body pid

twoPlayerTest :: DeckList -> (PlayerId -> PlayerId -> GameT ()) -> IO ()
twoPlayerTest deck body = Test.twoPlayerTest deck $ \one two -> run (DoStep 2 Setup) >> body one two

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "missing enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies)

passAttackActions :: GameT ()
passAttackActions = do
  asked <- getsGame (Map.toList . (.question))
  for_ (headMay [pid | (pid, QuestionLabel "$label.takeAction" _ _ _) <- asked]) $ \pid -> do
    passActions pid
    passAttackActions

defendAttack :: PlayerId -> Target -> GameT ()
defendAttack pid target = passAttackActions >> chooseTarget pid target >> passAttackActions

strongDefender :: PlayerId -> GameT Target
strongDefender pid = do
  target <- heroOf pid "01012"
  run $ CreateEffect $ Effect GameSource target [DefenseModifier 10] UntilEndOfRound
  pure target

spec :: Spec
spec = describe "Prancing Pony brigands" do
  describe "Lawless Ruffian" do
    it "checks the engaged player's current threat and loses its bonus while blank" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ RaiseThreat pid 8
      getThreat pid `shouldReturn` 35
      getEnemyAttack eid `shouldReturn` 3
      run $ EngageEnemy pid eid
      getEnemyAttack eid `shouldReturn` 5
      run $ RaiseThreat pid (-1)
      getEnemyAttack eid `shouldReturn` 3
      run $ RaiseThreat pid 1
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      getEnemyAttack eid `shouldReturn` 3
      run $ EndEffects UntilEndOfRound
      getEnemyAttack eid `shouldReturn` 5

    it "ignores a different player's high threat" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ RaiseThreat one 8
      run $ EngageEnemy two eid
      getEnemyAttack eid `shouldReturn` 3
      run $ EngageEnemy one eid
      getEnemyAttack eid `shouldReturn` 5

    it "schedules a single additional attack with a fresh shadow card" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      defender <- strongDefender pid
      another <- heroOf pid "01007"
      _ <- setEncounterDeck [Enemies.lawlessRuffian, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      defendAttack pid another
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion pid `shouldReturn` Nothing

  describe "Greenway Footpad" do
    it "raises every player's threat when that revelation option is selected" . twoPlayerTest testDeck $ \one two -> do
      _ <- setEncounterDeck [Enemies.greenwayFootpad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.prancingPony.raiseEachThreat2" one
      getThreat one `shouldReturn` 29
      getThreat two `shouldReturn` 29
      getQuestion one `shouldReturn` Nothing

    it "attacks the first player from staging without raising threat" . twoPlayerTest testDeck $ \one two -> do
      defender <- strongDefender one
      _ <- setEncounterDeck [Enemies.greenwayFootpad, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.prancingPony.immediateAttack" one
      defendAttack one defender
      eid <- getsGame (toId . fromJustNote "missing footpad" . headMay . toList . (.entities.enemies))
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      getThreat one `shouldReturn` 27
      getThreat two `shouldReturn` 27
      length <$> getEncounterDeck `shouldReturn` 1

    it "does not reveal when put directly into staging" . gameTest $ \pid -> do
      _ <- stageEnemy Enemies.greenwayFootpad
      getThreat pid `shouldReturn` 27
      getQuestion pid `shouldReturn` Nothing

    for_ [34, 35] $ \threat ->
      it ("uses defending-player threat " <> show threat <> " for its attack-scoped shadow") . twoPlayerTest testDeck $ \one two -> do
        eid <- stageEnemy Enemies.forestSpider
        shadow <- stageEnemy Enemies.greenwayFootpad
        cid <- (.cardId) . toAttrs <$> getEnemy shadow
        run $ RaiseThreat one (threat - 27)
        run $ RaiseThreat two (if threat == 34 then 8 else 0)
        run $ ResolveShadowEffect cid eid one []
        getEnemyAttack eid `shouldReturn` if threat == 34 then 3 else 4
        run $ EndEffects UntilEndOfAttack
        getEnemyAttack eid `shouldReturn` 2

  describe "Chetwood Outlaw" do
    it "raises the optionally engaging player's threat, not the first player's" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.chetwoodOutlaw
      run $ OptionalEngagement two
      chooseTarget two eid
      getThreat one `shouldReturn` 27
      getThreat two `shouldReturn` 31

    it "does not penalize an ordinary engagement" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.chetwoodOutlaw
      run $ EngageEnemy pid eid
      getThreat pid `shouldReturn` 27

    for_ [False, True] $ \returnEnemy ->
      it ("resolves its shadow choice after the current attack, return: " <> show returnEnemy) . gameTest $ \pid -> do
        eid <- stageEnemy Enemies.forestSpider
        run $ EngageEnemy pid eid
        defender <- strongDefender pid
        _ <- setEncounterDeck [Enemies.chetwoodOutlaw, Locations.oldForestRoad]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        passAttackActions
        chooseTarget pid defender
        passAttackActions
        (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith pid
        chooseLabel (if returnEnemy then "$label.prancingPony.returnAfterAttack" else "$label.prancingPony.raiseThreat2") pid
        passAttackActions
        (.placement) . toAttrs <$> getEnemy eid `shouldReturn` if returnEnemy then StagingArea else EngagedWith pid
        getThreat pid `shouldReturn` if returnEnemy then 27 else 29
