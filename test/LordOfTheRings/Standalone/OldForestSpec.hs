module LordOfTheRings.Standalone.OldForestSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (IsCard (toCard), CardType (EventType))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, CardMatcher (AnyCard))
import LordOfTheRings.Modifier (ModifierType (AttackModifier, AdditionalStagingReveals, PlayersCannotWin, MustBeTraveledTo, CannotPlay, PlayedCardThisRound))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (victoryLocations, locationVictoryPoints)
import LordOfTheRings.Window (WindowType (EnemyAttacking, ActiveLocationExplored, OptionallyEngaged), mkWhen, mkAfter)
import TestImport

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "enemy missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

victoryLocation :: CardDef -> GameT ()
victoryLocation def = do
  lid <- stageLocation def
  run $ AddToVictoryDisplay (LocationTarget lid)

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "location missing" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations)

spec :: Spec
spec = describe "Old Forest" do
  describe "Withywindle" do
    it "prevents victory in either area and reveals an extra card only while active" . gameTest $ \_ -> do
      river <- stageLocation Locations.withywindle
      getModifiers GameTarget >>= (`shouldSatisfy` elem PlayersCannotWin)
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))
      run $ PlaceLocation river ActiveLocation
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))
      run $ AddToVictoryDisplay (LocationTarget river)
      getModifiers GameTarget >>= (`shouldSatisfy` notElem PlayersCannotWin)
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))

  describe "Misleading Path" do
    it "requires travel only when no active location exists" . gameTest $ \_ -> do
      path <- stageLocation Locations.misleadingPath
      road <- stageLocation Locations.oldForestRoad
      getModifiers path >>= (`shouldSatisfy` elem MustBeTraveledTo)
      run $ PlaceLocation road ActiveLocation
      getModifiers path >>= (`shouldSatisfy` notElem MustBeTraveledTo)

    it "discards every ally in only the defending player's hand" . twoPlayerTest testDeck $ \one two -> do
      enemy <- stageEnemy Enemies.forestSpider
      path <- genEncounterCard Locations.misleadingPath
      run $ AttachShadowCard enemy path
      allyOne <- putCardIntoHand one Allies.guardOfTheCitadel
      allyTwo <- putCardIntoHand one Allies.henamarthRiversong
      event <- putCardIntoHand one Events.aTestOfWill
      other <- putCardIntoHand two Allies.guardOfTheCitadel
      run $ ResolveShadowCard path.id enemy one []
      (.hand) <$> getPlayer one `shouldReturn` [event]
      (.discard) <$> getPlayer one >>= (`shouldSatisfy` (\xs -> all (`elem` xs) [allyOne, allyTwo]))
      (.hand) <$> getPlayer two `shouldReturn` [other]

  describe "Deep Gulley" do
    it "uses the modified attack of an optionally engaged enemy" . twoPlayerTest testDeck $ \one two -> do
      _ <- stageLocation Locations.deepGulley
      enemy <- stageEnemy Enemies.longTrailer
      run $ CreateEffect $ Effect GameSource (EnemyTarget enemy) [AttackModifier 3] UntilEndOfRound
      threat <- (.threat) <$> getPlayer two
      otherThreat <- (.threat) <$> getPlayer one
      run $ EngageEnemy two enemy
      (.threat) <$> getPlayer two `shouldReturn` threat
      run $ CheckWindows [mkAfter $ OptionallyEngaged enemy two]
      (.threat) <$> getPlayer two `shouldReturn` threat + 7
      (.threat) <$> getPlayer one `shouldReturn` otherThreat

    it "does not raise threat once it is active" . gameTest $ \pid -> do
      gulley <- stageLocation Locations.deepGulley
      run $ PlaceLocation gulley ActiveLocation
      enemy <- stageEnemy Enemies.forestSpider
      threat <- (.threat) <$> getPlayer pid
      run $ CheckWindows [mkAfter $ OptionallyEngaged enemy pid]
      (.threat) <$> getPlayer pid `shouldReturn` threat

  describe "Strange Woods" do
    it "prevents healing every player's characters only while staged" . twoPlayerTest testDeck $ \one two -> do
      woods <- stageLocation Locations.strangeWoods
      heroes <- traverse (\pid -> heroOf pid "01012") [one, two]
      for_ heroes \hero -> do
        run $ DealDamage GameSource hero 2
        run $ HealDamage GameSource hero 2
        getDamage hero `shouldReturn` 2
      run $ PlaceLocation woods ActiveLocation
      for_ heroes \hero -> do
        run $ HealDamage GameSource hero 2
        getDamage hero `shouldReturn` 0

    for_ [False, True] \destroyed ->
      it ("raises everyone's threat from its shadow only if the attack destroys a character: " <> show destroyed) . twoPlayerTest testDeck $ \one two -> do
        enemy <- stageEnemy Enemies.longTrailer
        card <- genEncounterCard Locations.strangeWoods
        run $ AttachShadowCard enemy card
        defender <- if destroyed then putAllyIntoPlay one Allies.guardOfTheCitadel else heroOf one "01012"
        threats <- traverse (fmap (.threat) . getPlayer) [one, two]
        run $ EnemyAttack one enemy
        passActions one
        passActions two
        chooseTarget one defender
        passActions one
        passActions two
        passActions one
        passActions two
        traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` map (+ if destroyed then 2 else 0) threats

  describe "Wall of Trees" do
    it "leaves active-location progress alone and takes progress before the current quest" . twoPlayerTest testDeck $ \_ _ -> do
      quest <- putQuestStage Quests.fliesAndSpiders
      wall <- stageLocation Locations.wallOfTrees
      road <- stageLocation Locations.oldForestRoad
      run $ PlaceLocation road ActiveLocation
      getLocationThreat wall `shouldReturn` 2
      run $ PlaceTokens GameSource (LocationTarget road) Progress 1
      (.progress) . toAttrs <$> getLocation wall `shouldReturn` 0
      run $ PlaceTokens GameSource quest Progress 3
      (.progress) . toAttrs <$> getLocation wall `shouldReturn` 3
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0
      run $ PlaceTokens GameSource quest Progress 3
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 2
      cards <- victoryLocations
      map toCardCode cards `shouldBe` [toCardCode Locations.wallOfTrees]
      traverse locationVictoryPoints cards `shouldReturn` [2]

    it "lets the first player order multiple walls and spills excess progress onto the quest" . gameTest $ \pid -> do
      quest <- putQuestStage Quests.fliesAndSpiders
      wallOne <- stageLocation Locations.wallOfTrees
      wallTwo <- stageLocation Locations.wallOfTrees
      run $ PlaceTokens GameSource quest Progress 9
      chooseTarget pid (LocationTarget wallTwo)
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 1
      getsGame (map toId . toList . (.entities.locations)) >>= (`shouldSatisfy` (\xs -> all (`notElem` xs) [wallOne, wallTwo]))
      cards <- victoryLocations
      length cards `shouldBe` 2
      traverse locationVictoryPoints cards `shouldReturn` [1, 1]

  describe "Oppressive Forest" do
    it "counts cards played before it arrived and limits only the player who has played" . twoPlayerTest testDeck $ \one two -> do
      run $ CreateEffect $ Effect GameSource (PlayerTarget one) [PlayedCardThisRound (toCardCode Events.aTestOfWill) EventType] UntilEndOfRound
      forest <- stageLocation Locations.oppressiveForest
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` elem (CannotPlay AnyCard))
      getModifiers (PlayerTarget two) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))
      run $ EndEffects UntilEndOfRound
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))
      run $ CreateEffect $ Effect GameSource (PlayerTarget one) [PlayedCardThisRound (toCardCode Events.aTestOfWill) EventType] UntilEndOfRound
      run $ PlaceLocation forest ActiveLocation
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))

    for_ [False, True] \staged ->
      it ("adds its shadow to staging only when no locations are staged: " <> show staged) . gameTest $ \pid -> do
        when staged $ void $ stageLocation Locations.oldForestRoad
        enemy <- stageEnemy Enemies.forestSpider
        card <- genEncounterCard Locations.oppressiveForest
        run $ AttachShadowCard enemy card
        run $ ResolveShadowCard card.id enemy pid []
        getsGame (any ((== toCardCode Locations.oppressiveForest) . toCardCode) . toList . (.entities.locations)) `shouldReturn` not staged

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
