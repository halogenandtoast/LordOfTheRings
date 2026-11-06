module LordOfTheRings.Standalone.OldForestSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Window (WindowType (EnemyAttacking, ActiveLocationExplored), mkWhen, mkAfter)
import TestImport

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "enemy missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

victoryLocation :: CardDef -> GameT ()
victoryLocation def = do
  run . AddToStaging =<< genEncounterCard def
  lid <- getsGame $ toId . fromJustNote "location missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations)
  run $ AddToVictoryDisplay (LocationTarget lid)

spec :: Spec
spec = describe "Old Forest" do
  describe "Old Man Willow" do
    it "discards a location from victory and raises only the defending player's threat by its points" . twoPlayerTest testDeck $ \one two -> do
      willow <- stageEnemy Enemies.oldManWillow
      troll <- stageEnemy Enemies.hillTroll
      run $ AddToVictoryDisplay (EnemyTarget troll)
      victoryLocation Locations.theStoneOfErechStandalone
      threatOne <- (.threat) <$> getPlayer one
      threatTwo <- (.threat) <$> getPlayer two
      run $ UseCardAbility two (EnemySource willow) 1 [mkWhen $ EnemyAttacking willow two]
      (.threat) <$> getPlayer one `shouldReturn` threatOne
      (.threat) <$> getPlayer two `shouldReturn` threatTwo + 5
      cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      map toCardCode cards `shouldBe` [toCardCode Enemies.hillTroll]
      getEncounterDiscard >>= (\xs -> map toCardCode xs `shouldSatisfy` elem (toCardCode Locations.theStoneOfErechStandalone))

    it "does nothing when victory contains no locations" . gameTest $ \pid -> do
      willow <- stageEnemy Enemies.oldManWillow
      threat <- (.threat) <$> getPlayer pid
      run $ UseCardAbility pid (EnemySource willow) 1 [mkWhen $ EnemyAttacking willow pid]
      (.threat) <$> getPlayer pid `shouldReturn` threat
      getQuestion pid `shouldReturn` Nothing

    it "remains in play with damage greater than its hit points" . gameTest $ \_ -> do
      willow <- stageEnemy Enemies.oldManWillow
      run $ DealDamage GameSource (EnemyTarget willow) 20
      getDamage (EnemyTarget willow) `shouldReturn` 20
      (.placement) <$> getEnemy willow `shouldReturn` StagingArea

  describe "Grasping Root" do
    it "assigns damage equal to the number of victory locations rather than their points" . twoPlayerTest testDeck $ \one two -> do
      root <- stageEnemy Enemies.graspingRoot
      victoryLocation Locations.theStoneOfErechStandalone
      victoryLocation Locations.tarlangsNeck
      troll <- stageEnemy Enemies.hillTroll
      run $ AddToVictoryDisplay (EnemyTarget troll)
      beravor <- heroOf one "01012"
      eowyn <- heroOf one "01007"
      other <- heroOf two "01012"
      run $ EngageEnemy one root
      chooseTarget one beravor
      chooseTarget one eowyn
      getDamage beravor `shouldReturn` 1
      getDamage eowyn `shouldReturn` 1
      getDamage other `shouldReturn` 0

    it "assigns no damage with an empty victory display" . gameTest $ \pid -> do
      root <- stageEnemy Enemies.graspingRoot
      run $ EngageEnemy pid root
      getQuestion pid `shouldReturn` Nothing

  describe "Army of Flies" do
    it "discards an attachment controlled by the engaged player" . twoPlayerTest testDeck $ \one two -> do
      flies <- stageEnemy Enemies.armyOfFlies
      heroOne <- heroOf one "01012"
      heroTwo <- heroOf two "01012"
      own <- putAttachmentOn one Attachments.resourceful heroOne
      other <- putAttachmentOn two Attachments.resourceful heroTwo
      run $ EngageEnemy one flies
      getsGame (elem own . map toTarget . toList . (.entities.attachments)) `shouldReturn` False
      getsGame (elem other . map toTarget . toList . (.entities.attachments)) `shouldReturn` True

    it "also discards a defending player's attachment as a shadow effect" . gameTest $ \pid -> do
      flies <- stageEnemy Enemies.armyOfFlies
      cid <- (.id) . toCard <$> getEnemy flies
      hero <- heroOf pid "01012"
      attachment <- putAttachmentOn pid Attachments.resourceful hero
      run $ ResolveShadowEffect cid flies pid [hero]
      getsGame (elem attachment . map toTarget . toList . (.entities.attachments)) `shouldReturn` False

  describe "Long Trailer" do
    it "attacks the first player from staging when the active location is explored" . twoPlayerTest testDeck $ \one two -> do
      trailer <- stageEnemy Enemies.longTrailer
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame $ toId . fromJustNote "location missing" . headMay . toList . (.entities.locations)
      defender <- heroOf one "01012"
      other <- heroOf two "01012"
      run $ CheckWindows [mkAfter $ ActiveLocationExplored lid]
      passActions one
      passActions two
      chooseTarget one defender
      passActions one
      passActions two
      passActions one
      passActions two
      getDamage defender `shouldReturn` 2
      getDamage other `shouldReturn` 0
      (.placement) <$> getEnemy trailer `shouldReturn` StagingArea

    it "does not make the attack while engaged" . gameTest $ \pid -> do
      trailer <- stageEnemy Enemies.longTrailer
      run $ EngageEnemy pid trailer
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame $ toId . fromJustNote "location missing" . headMay . toList . (.entities.locations)
      run $ CheckWindows [mkAfter $ ActiveLocationExplored lid]
      getQuestion pid `shouldReturn` Nothing
