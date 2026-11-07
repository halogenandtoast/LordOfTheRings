module LordOfTheRings.Standalone.AnnuminasLocationsSpec (spec, stageLocation) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getEngagementCost)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getThreat, getMainQuestProgress)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Standalone.AnnuminasInvadersSpec (mainStage)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

annuminasTest :: (PlayerId -> GameT ()) -> IO ()
annuminasTest body = gameTest $ \pid -> run (DoStep 2 Setup) >> body pid

annuminasTwo :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
annuminasTwo body = twoPlayerTest testDeck $ \one two -> run (DoStep 2 Setup) >> body one two

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "missing location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations))

reload :: GameT ()
reload = do
  current <- getGame
  case eitherDecode (encode current) of
    Left err -> liftIO $ expectationFailure err
    Right restored -> putGame =<< preloadModifiers restored

spec :: Spec
spec = describe "Annúminas countryside locations" do
  describe "Gate of Annúminas" do
    it "counts both engaged and staged enemies for its printed X, and loses X while blank" . annuminasTest $ \pid -> do
      lid <- stageLocation Locations.gateOfAnnuminas
      getLocationThreat lid `shouldReturn` 1
      eid <- stageEnemy Enemies.lawlessRuffian
      _ <- stageEnemy Enemies.chetwoodOutlaw
      run $ EngageEnemy pid eid
      getLocationThreat lid `shouldReturn` 3
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
      getLocationThreat lid `shouldReturn` 0

    for_ [False, True] $ \rotated ->
      it ("makes each player engage a different enemy in first-player order, rotated: " <> show rotated) . annuminasTwo $ \one two -> do
        lid <- stageLocation Locations.gateOfAnnuminas
        firstEnemy <- stageEnemy Enemies.lawlessRuffian
        getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
        secondEnemy <- stageEnemy Enemies.lawlessRuffian
        getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
        when rotated $ run $ SetFirstPlayer two
        let former = if rotated then two else one
            latter = if rotated then one else two
        run $ TravelTo lid
        getQuestion latter `shouldReturn` Nothing
        reload
        chooseTarget former firstEnemy
        (.placement) . toAttrs <$> getEnemy firstEnemy `shouldReturn` EngagedWith former
        (.placement) . toAttrs <$> getEnemy secondEnemy `shouldReturn` EngagedWith latter
        (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

    it "rejects a numerical surplus of enemies if one player cannot engage any of them" . annuminasTwo $ \_ two -> do
      lid <- stageLocation Locations.gateOfAnnuminas
      enemies <- replicateM 2 (stageEnemy Enemies.lawlessRuffian) :: GameT [EnemyId]
      for_ enemies $ \eid -> run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotEngagePlayer two] UntilEndOfRound
      getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ TravelTo lid
      (.placement) <$> getLocation lid `shouldReturn` StagingArea

    it "offers only engagements that leave the remaining player a legal enemy" . annuminasTwo $ \one two -> do
      lid <- stageLocation Locations.gateOfAnnuminas
      reserved <- stageEnemy Enemies.lawlessRuffian
      shared <- stageEnemy Enemies.lawlessRuffian
      run $ CreateEffect $ Effect GameSource (EnemyTarget reserved) [EngagesOnly two] UntilEndOfRound
      getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo lid
      (.placement) . toAttrs <$> getEnemy shared `shouldReturn` EngagedWith one
      (.placement) . toAttrs <$> getEnemy reserved `shouldReturn` EngagedWith two
      (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

    it "does not count an enemy considered out of play toward the travel cost" . annuminasTest $ \_ -> do
      lid <- stageLocation Locations.gateOfAnnuminas
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [ConsideredOutOfPlay] UntilEndOfRound
      getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ TravelTo lid
      (.placement) <$> getLocation lid `shouldReturn` StagingArea

  describe "Arnor Battlefield" do
    it "stacks its engagement reduction for non-unique enemies and stops applying when active or blank" . annuminasTest $ \_ -> do
      former <- stageLocation Locations.arnorBattlefield
      latter <- stageLocation Locations.arnorBattlefield
      eid <- stageEnemy Enemies.lawlessRuffian
      unique <- stageEnemy Enemies.toddTheTroll
      getEngagementCost eid `shouldReturn` 25
      getEngagementCost unique `shouldReturn` 45
      run $ PlaceLocation former ActiveLocation
      getEngagementCost eid `shouldReturn` 30
      run $ CreateEffect $ Effect GameSource (LocationTarget latter) [Blank] UntilEndOfRound
      getEngagementCost eid `shouldReturn` 35

    it "makes each player damage one of their own heroes to travel" . annuminasTwo $ \one two -> do
      lid <- stageLocation Locations.arnorBattlefield
      former <- heroOf one "01012"
      latter <- heroOf two "01007"
      idle <- heroOf one "01007"
      run $ TravelTo lid
      chooseTarget one former
      chooseTarget two latter
      getDamage former `shouldReturn` 1
      getDamage latter `shouldReturn` 1
      getDamage idle `shouldReturn` 0
      (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

    it "cannot be traveled to when a player has no hero that can take the travel damage" . annuminasTwo $ \one _ -> do
      lid <- stageLocation Locations.arnorBattlefield
      for_ ["01012", "01007", "01002"] $ \code -> do
        hero <- heroOf one code
        run $ CreateEffect $ Effect GameSource hero [CannotTakeDamage] UntilEndOfRound
      getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ TravelTo lid
      (.placement) <$> getLocation lid `shouldReturn` StagingArea

  describe "Hills of Evendim" do
    it "blocks when-revealed cancellation only while staged" . annuminasTest $ \pid -> do
      lid <- stageLocation Locations.hillsOfEvendim
      cancellation <- putCardIntoHand pid Events.aTestOfWill
      payer <- heroOf pid "01007"
      run $ PlaceTokens GameSource payer Resource 1
      _ <- setEncounterDeck [Treacheries.deadlyAccusations, Treacheries.deadlyAccusations, Locations.oldForestRoad]
      run RevealEncounterCard
      offered <- maybe [] questionChoices <$> getQuestion pid
      [target | TargetLabel target _ <- offered] `shouldSatisfy` notElem (CardIdTarget cancellation.id)
      resolveRevealed
      getThreat pid `shouldReturn` 28
      run $ PlaceLocation lid ActiveLocation
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget cancellation.id)
      resolveRevealed
      getThreat pid `shouldReturn` 28

    it "requires an enemy in the deck or discard, shuffles the discard first and stages the actual enemy without revealing it" . annuminasTest $ \pid -> do
      lid <- stageLocation Locations.hillsOfEvendim
      _ <- setEncounterDeck [Treacheries.deadlyAccusations]
      getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      found <- genEncounterCard Enemies.greenwayFootpad
      run $ AddToEncounterDiscard [found]
      getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo lid
      getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [found.id]
      getThreat pid `shouldReturn` 27
      (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  describe "Shores of Lake Nenuial" do
    it "blocks progress on other staged locations, while its own progress, active locations and the quest remain available" . annuminasTest $ \_ -> do
      stage <- mainStage 0
      shore <- stageLocation Locations.shoresOfLakeNenuial
      staged <- stageLocation Locations.gateOfAnnuminas
      active <- stageLocation Locations.hillsOfEvendim
      run $ PlaceLocation active ActiveLocation
      for_ [LocationTarget shore, LocationTarget staged, LocationTarget active, stage] $ \target -> run $ PlaceTokens GameSource target Progress 1
      getDamage (LocationTarget shore) `shouldReturn` 0
      (.progress) <$> getLocation shore `shouldReturn` 1
      (.progress) <$> getLocation staged `shouldReturn` 0
      (.progress) <$> getLocation active `shouldReturn` 1
      getMainQuestProgress `shouldReturn` 1

    it "has two Shores prevent progress on each other, and blanking one removes only its restriction" . annuminasTest $ \_ -> do
      former <- stageLocation Locations.shoresOfLakeNenuial
      latter <- stageLocation Locations.shoresOfLakeNenuial
      run $ PlaceTokens GameSource (LocationTarget former) Progress 1
      run $ PlaceTokens GameSource (LocationTarget latter) Progress 1
      (.progress) <$> getLocation former `shouldReturn` 0
      (.progress) <$> getLocation latter `shouldReturn` 0
      run $ CreateEffect $ Effect GameSource (LocationTarget former) [Blank] UntilEndOfRound
      run $ PlaceTokens GameSource (LocationTarget latter) Progress 1
      (.progress) <$> getLocation latter `shouldReturn` 1

    it "raises every player's threat by two to travel and stops restricting staged progress" . annuminasTwo $ \one two -> do
      shore <- stageLocation Locations.shoresOfLakeNenuial
      other <- stageLocation Locations.gateOfAnnuminas
      run $ TravelTo shore
      getThreat one `shouldReturn` 29
      getThreat two `shouldReturn` 29
      run $ PlaceTokens GameSource (LocationTarget other) Progress 1
      (.progress) <$> getLocation other `shouldReturn` 1
