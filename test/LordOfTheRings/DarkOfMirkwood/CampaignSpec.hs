module LordOfTheRings.DarkOfMirkwood.CampaignSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Scenario.Scenarios.TheCavesOfNibinDum (theCavesOfNibinDum)
import LordOfTheRings.Scenario.Scenarios.TheOath (theOath)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Matcher (getThreat)
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (RoundEnds))
import TestImport

mini :: CampaignLog
mini = newCampaignLog "dark-of-mirkwood"

spec :: Spec
spec = describe "The Dark of Mirkwood campaign" do
  it "adds Stalking Goblin to the pool and encounter deck at setup" . campaignTestIn theOath mini starterDeck $ \_ -> do
    run $ DoStep 2 Setup
    map (.code)
      . (.pool)
      <$> getCampaignLog
      >>= (`shouldSatisfy` elem (toCardCode Enemies.stalkingGoblin))
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Enemies.stalkingGoblin))

  it "records a chosen Adaptation on its hero for later scenarios" . campaignTestIn theOath mini starterDeck $ \pid -> do
    run $ DoStep 4 Setup
    chooseOptionMatching "choose the first Adaptation" pid (const True)
    hero <- getHeroWithCode "01001"
    chooseTarget pid hero.id
    l <- getCampaignLog
    l.pool `shouldBe` [PoolCard (toCardCode Attachments.theFervorOfLore) Nothing (Just "01001")]

  it "earns Guilty Conscience at six resources" . campaignTestIn theOath mini {campaignLogCardResources = 6} starterDeck $ \_ -> do
    run ResolveCampaign
    map (.code)
      . (.pool)
      <$> getCampaignLog
      >>= (`shouldSatisfy` elem (toCardCode Treacheries.guiltyConscience))

  it "avoids Guilty Conscience at five resources" . campaignTestIn theOath mini {campaignLogCardResources = 5} starterDeck $ \_ -> do
    run ResolveCampaign
    map (.code)
      . (.pool)
      <$> getCampaignLog
      >>= (`shouldSatisfy` notElem (toCardCode Treacheries.guiltyConscience))

  it "puts No Time to Waste in hand and earns Shiny Distraction" . campaignTestIn theCavesOfNibinDum mini starterDeck $ \pid -> do
    run $ DoStep 2 Setup
    run $ DoStep 4 Setup
    map toCardCode
      . (.hand)
      <$> getPlayer pid
      >>= (`shouldSatisfy` elem (toCardCode Events.noTimeToWaste))
    l <- getCampaignLog
    l.pool `shouldSatisfy` elem (PoolCard (toCardCode Events.noTimeToWaste) (Just 0) Nothing)
    map (.code) l.pool `shouldSatisfy` elem (toCardCode Treacheries.shinyDistraction)

  it "removes only the used copy of No Time to Waste from discard and the pool"
    . campaignTestIn
      theCavesOfNibinDum
      mini
        { campaignLogPool =
            [ PoolCard (toCardCode Events.noTimeToWaste) (Just 0) Nothing
            , PoolCard (toCardCode Events.noTimeToWaste) (Just 1) Nothing
            ]
        }
      starterDeck
    $ \pid -> do
      card <- genPlayerCard Events.noTimeToWaste (Just pid)
      run $ PlayEvent pid card []
      chooseLabel "$label.campaign.noTimeToWaste" pid
      player <- getPlayer pid
      map (.id) player.discard `shouldSatisfy` notElem card.id
      l <- getCampaignLog
      l.pool `shouldBe` [PoolCard (toCardCode Events.noTimeToWaste) (Just 1) Nothing]

  it "restores an Adaptation on its recorded hero in the Caves" . campaignTestIn
    theCavesOfNibinDum
    mini {campaignLogPool = [PoolCard (toCardCode Attachments.thePowerOfSpirit) Nothing (Just "01001")]}
    starterDeck $ \_ -> do
    run $ DoStep 4 Setup
    hero <- getHeroWithCode "01001"
    attachments <- getsGame (filter ((== toCardCode Attachments.thePowerOfSpirit) . toCardCode) . toList . (.entities.attachments))
    length attachments `shouldBe` 1
    for_ attachments $ \a -> a.placement `shouldBe` AttachedTo (toTarget hero)

  it "does not grant Ranger Sense when any player's final threat is 40" . campaignTestIn theCavesOfNibinDum mini starterDeck $ \pid -> do
    current <- getThreat pid
    run $ RaiseThreat pid (40 - current)
    run ResolveCampaign
    l <- getCampaignLog
    recorded RangerSenseNextCampaign l `shouldBe` []
    map (.code) l.pool `shouldSatisfy` notElem (toCardCode Events.rangerSense)

  it "counts a resource at the end of each round in campaign mode" . campaignTestIn theOath mini starterDeck $ \_ -> do
    run $ CheckWindows [Window When RoundEnds]
    (.cardResources) <$> getCampaignLog `shouldReturn` 1
    run $ CheckWindows [Window When RoundEnds]
    (.cardResources) <$> getCampaignLog `shouldReturn` 2
