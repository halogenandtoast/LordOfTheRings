module LordOfTheRings.Standalone.FogCampaignSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Campaign qualified as Campaign
import LordOfTheRings.Campaign.CardDefs qualified as Campaigns
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (getActionAbilities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotLoseFirstPlayerToken))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Saga (sagaOfMode, fellowshipSetupForMode, FellowshipSetup (..))
import LordOfTheRings.Scenario (fogOnTheBarrowDowns)
import LordOfTheRings.Standalone.FogBarrowsSpec (homeStage, enterBarrow, returnFromBarrow)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

fogLog :: Campaign.CampaignLog
fogLog = (Campaign.newCampaignLog "the-lord-of-the-rings-with-fog-on-the-barrow-downs")
  { Campaign.campaignLogCompleted = [Campaign.CompletedScenario "a-shadow-of-the-past" 0] }

earnedLog :: Campaign.CampaignLog
earnedLog = Campaign.addToPool (Campaign.PoolCard (toCardCode Events.hoTomBombadil) Nothing Nothing) fogLog

finishSetup :: Int -> GameT ()
finishSetup n | n <= 0 = error "Fog setup did not finish"
finishSetup n = do
  questions <- getsGame (Map.toList . (.question))
  for_ (headMay questions) $ \(pid, _) -> do
    chooseOptionMatching "complete setup" pid (const True)
    finishSetup (n - 1)

hoTomInHand :: PlayerId -> GameT Card
hoTomInHand pid = fromJustNote "Ho! Tom Bombadil! missing" . find ((== toCardCode Events.hoTomBombadil) . toCardCode) . (.hand) <$> getPlayer pid

spec :: Spec
spec = describe "Fog campaign" do
  it "offers all four Saga routes in the printed order" do
    let stages = map (map fst . Campaign.campaignDefScenarios)
          [Campaign.theLordOfTheRings, Campaign.theLordOfTheRingsWithOldForest, Campaign.theLordOfTheRingsWithFog, Campaign.theLordOfTheRingsWithOldForestAndFog]
    map length stages `shouldBe` [18, 19, 19, 20]
    map (take 4) stages `shouldBe`
      [ ["a-shadow-of-the-past", "a-knife-in-the-dark", "flight-to-the-ford", "the-ring-goes-south"]
      , ["a-shadow-of-the-past", "the-old-forest", "a-knife-in-the-dark", "flight-to-the-ford"]
      , ["a-shadow-of-the-past", "fog-on-the-barrow-downs", "a-knife-in-the-dark", "flight-to-the-ford"]
      , ["a-shadow-of-the-past", "the-old-forest", "fog-on-the-barrow-downs", "a-knife-in-the-dark"]
      ]
    Campaign.nextScenario fogLog `shouldBe` Just "fog-on-the-barrow-downs"
    Campaign.campaignCardFor fogLog "fog-on-the-barrow-downs" `shouldBe` Just (toCardCode Campaigns.fogOnTheBarrowDownsCampaign) :: IO ()

  it "uses Fellowship setup only in campaign mode" do
    sagaOfMode "fog-on-the-barrow-downs" False `shouldBe` Nothing
    isNothing (fellowshipSetupForMode "fog-on-the-barrow-downs" False) `shouldBe` True
    let components setup = (fellowshipHeroes setup, fellowshipRing setup)
    fmap components (fellowshipSetupForMode "fog-on-the-barrow-downs" True) `shouldBe` fmap components (fellowshipSetupForMode "a-shadow-of-the-past" True) :: IO ()

  it "sets up Black Riders Frodo and the Ring in campaign mode" . campaignTestIn fogOnTheBarrowDowns fogLog testDeck $ \_ -> do
    run Setup
    finishSetup 100
    getsGame (length . (.entities.heroes)) `shouldReturn` 4
    getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` elem "141001")
    getsGame (map toCardCode . toList . (.entities.objectives)) >>= (`shouldSatisfy` elem "141085")

  it "does not require Saga components in standalone setup" . gameTestIn fogOnTheBarrowDowns testDeck $ \_ -> do
    run Setup
    finishSetup 100
    getsGame (length . (.entities.heroes)) `shouldReturn` 3
    getsGame (null . (.entities.objectives)) `shouldReturn` True

  it "locks the actual first player token while another player's stage 4 is held out of focus" . twoPlayerCampaignTestIn fogOnTheBarrowDowns fogLog testDeck $ \one two -> do
    _ <- homeStage
    enterBarrow two
    run $ FocusArea one
    getModifiers (PlayerTarget one) >>= (`shouldSatisfy` elem CannotLoseFirstPlayerToken)
    run $ SetFirstPlayer two
    getLead `shouldReturn` one
    getsGame (fmap separationFirstPlayer . gameSeparation) `shouldReturn` Just one
    returnFromBarrow two
    getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem CannotLoseFirstPlayerToken)
    run $ SetFirstPlayer two
    getLead `shouldReturn` two

  it "enforces the global token lock from the private area too" . twoPlayerCampaignTestIn fogOnTheBarrowDowns fogLog testDeck $ \one two -> do
    _ <- homeStage
    enterBarrow two
    getLead `shouldReturn` two
    run $ SetFirstPlayer two
    getsGame (fmap separationFirstPlayer . gameSeparation) `shouldReturn` Just one

  it "has no campaign token lock in standalone mode" . twoPlayerTestIn fogOnTheBarrowDowns testDeck $ \one two -> do
    _ <- homeStage
    enterBarrow two
    getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem CannotLoseFirstPlayerToken)
    run $ SetFirstPlayer two
    getsGame (fmap separationFirstPlayer . gameSeparation) `shouldReturn` Just two

  it "awards the single shared boon on victory and advances the Saga" . campaignTestIn fogOnTheBarrowDowns fogLog testDeck $ \_ -> do
    run $ GameOver Won
    logbook <- getCampaignLog
    map Campaign.poolCardCode logbook.pool `shouldBe` [toCardCode Events.hoTomBombadil]
    map Campaign.poolCardSeat logbook.pool `shouldBe` [Nothing]
    Campaign.nextScenario logbook `shouldBe` Just "a-knife-in-the-dark"
    Campaign.seatPoolCards 0 logbook `shouldBe` []

  describe "Ho! Tom Bombadil!" do
    it "goes to the current first player's hand before the ordinary hand and survives a mulligan" . twoPlayerCampaignTestIn passageThroughMirkwood earnedLog testDeck $ \one two -> do
      run $ SetFirstPlayer two
      run $ DoStep 4 Setup
      boon <- hoTomInHand two
      getsGame (length . (.hand) . fromJustNote "missing first player" . lookup two . (.players)) `shouldReturn` 1
      length . (.hand) <$> getPlayer one `shouldReturn` 0
      run $ DoStep 5 Setup
      -- Player actors are traversed by id, so either seat can be asked first.
      replicateM_ 2 do
        asked <- getsGame (fst . fromJustNote "missing starting-hand question" . headMay . Map.toList . (.question))
        chooseLabel (if asked == two then "$label.mulligan" else "$label.keepHand") asked
      length . (.hand) <$> getPlayer two `shouldReturn` 7
      (.id) <$> hoTomInHand two `shouldReturn` boon.id

    it "cancels only the when-revealed effect, preserves surge and doomed, and leaves the campaign pool" . campaignTestIn passageThroughMirkwood earnedLog testDeck $ \pid -> do
      run $ DoStep 4 Setup
      boon <- hoTomInHand pid
      initial <- (.threat) <$> getPlayer pid
      _ <- setEncounterDeck [Treacheries.frozenByFear, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget boon.id)
      resolveRevealed
      (.threat) <$> getPlayer pid `shouldReturn` initial + 1
      getActionAbilities pid >>= (`shouldSatisfy` notNull)
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.oldForestRoad]
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Events.hoTomBombadil]
      Campaign.campaignLogPool <$> getCampaignLog `shouldReturn` []
      length . (.hand) <$> getPlayer pid `shouldReturn` 0
      map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` []

    it "cannot cancel Tom Bombadil's uncancelable when-revealed effect" . campaignTestIn passageThroughMirkwood earnedLog testDeck $ \pid -> do
      run $ DoStep 4 Setup
      boon <- hoTomInHand pid
      _ <- setEncounterDeck [Objectives.tomBombadilFog, Locations.oldForestRoad]
      run RevealEncounterCard
      asked <- getQuestion pid
      [target | Just question <- [asked], TargetLabel target _ <- questionChoices question] `shouldSatisfy` notElem (CardIdTarget boon.id)
      resolveRevealed
      (.id) <$> hoTomInHand pid `shouldReturn` boon.id
      length . Campaign.campaignLogPool <$> getCampaignLog `shouldReturn` 1

    it "does not appear before being earned" . campaignTestIn passageThroughMirkwood fogLog testDeck $ \pid -> do
      run $ DoStep 4 Setup
      length . (.hand) <$> getPlayer pid `shouldReturn` 0

    it "cannot pay its response cost if it is no longer in the campaign pool" . campaignTestIn passageThroughMirkwood fogLog testDeck $ \pid -> do
      boon <- putCardIntoHand pid Events.hoTomBombadil
      _ <- setEncounterDeck [Treacheries.frozenByFear, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      asked <- getQuestion pid
      [target | Just question <- [asked], TargetLabel target _ <- questionChoices question] `shouldSatisfy` notElem (CardIdTarget boon.id)
