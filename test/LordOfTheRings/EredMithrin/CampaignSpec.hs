module LordOfTheRings.EredMithrin.CampaignSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Card.CardDef (cdEncounterSetQuantity)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (IgnoreDeep, IgnoreDiscover))
import LordOfTheRings.Outcome (Outcome (..))
import LordOfTheRings.Scenario (fireInTheNight, journeyUpTheAnduin, mountGundabad, theFateOfWilderland)
import TestImport

emLog :: CampaignLog
emLog = (newCampaignLog "ered-mithrin") {campaignLogHeroes = [["01012", "01007", "01002"]]}

wonFirst :: [ScenarioId] -> CampaignLog -> CampaignLog
wonFirst sids l = l {campaignLogCompleted = [CompletedScenario s 100 | s <- sids]}

-- | The log with every treasure discovered but Jewels of Wilderland.
allButJewels :: CampaignLog
allButJewels = record Treasures (filter (/= toCardCode Attachments.jewelsOfWilderland) eredMithrinTreasures) emLog

poolCodes :: CampaignLog -> [CardCode]
poolCodes = map (.code) . (.pool)

pureIt :: String -> IO () -> Spec
pureIt = it

spec :: Spec
spec = describe "Ered Mithrin campaign" do
  describe "the campaign log" do
    pureIt "plays the cycle's nine scenarios in order" do
      nextScenario emLog `shouldBe` Just "journey-up-the-anduin"
      nextScenario (wonFirst ["journey-up-the-anduin", "lost-in-mirkwood"] emLog) `shouldBe` Just "the-kings-quest"
      campaignCardFor emLog "mount-gundabad" `shouldBe` Just "EMC193"

    pureIt "bars the Haldan hero, save in The King's Quest" do
      seatHeroes [["01012", "01007", "19026"]] emLog `shouldBe` Left ["19026"]
      let kingsQuest = wonFirst ["journey-up-the-anduin", "lost-in-mirkwood"] emLog
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "19026"]] kingsQuest `shouldBe` Right 0

    pureIt "lets heroes change freely at The King's Quest and The Ghost of Framsburg" do
      let ghost = wonFirst ["journey-up-the-anduin", "lost-in-mirkwood", "the-kings-quest", "the-withered-heath", "roam-across-rhovanion", "fire-in-the-night"] emLog
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "01001"]] ghost `shouldBe` Right 0
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "01001"]] (wonFirst ["journey-up-the-anduin"] emLog) `shouldBe` Right 1

    pureIt "writes each treasure discovered once, the rest undiscovered" do
      let l = updateLog (MarkTreasure "EMC196") $ updateLog (MarkTreasure "EMC196") emLog
      recorded Treasures l `shouldBe` ["EMC196"]
      length (undiscoveredTreasures l) `shouldBe` 8

    pureIt "adds no more copies of an encounter card to the pool than a collection holds" do
      -- Hill Troll: 1 copy in Wilderlands
      let add = updateLog (AddEncounterCardToPool $ toCardCode Enemies.hillTroll)
          owned = fromMaybe 1 (cdEncounterSetQuantity Enemies.hillTroll)
      length (poolCodes (foldr (const add) emLog [0 .. owned])) `shouldBe` owned

    pureIt "takes one copy out of the pool at a time" do
      let card = PoolCard (toCardCode Locations.twilightHall) Nothing Nothing
          l = emLog {campaignLogPool = [card, card]}
      length (updateLog (RemoveOneFromPool card) l).pool `shouldBe` 1

  describe "Journey Up the Anduin" do
    it "offers Turayn and, declined, notes his oath; A Growing Threat and the Evil Creatures in play join the pool" . campaignTestIn journeyUpTheAnduin emLog testDeck $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.hillTroll
      run $ GameOver Won
      chooseLabel "$label.eredMithrin.allyDeclined" pid
      l <- getCampaignLog
      Map.member TuraynsOath l.notes `shouldBe` True
      poolCodes l `shouldBe` ["EMC167", toCardCode Enemies.hillTroll]

    it "puts a resource on the card for a non-unique location explored, then a treasure, discovered once won" . campaignTestIn journeyUpTheAnduin allButJewels {campaignLogCardResources = 3} testDeck $ \pid -> do
      run $ UseCardAbility pid ScenarioSource 1 []
      (.cardResources) <$> getCampaignLog `shouldReturn` 4
      chooseOptionMatching "choose the treasure" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      beravor <- getHeroWithCode "01012"
      chooseTarget pid beravor.id
      map toCardCode . toList <$> getsGame (.entities.attachments) `shouldReturn` [toCardCode Attachments.jewelsOfWilderland]
      run $ GameOver Won
      chooseLabel "$label.eredMithrin.allyDeclined" pid
      l <- getCampaignLog
      toCardCode Attachments.jewelsOfWilderland `elem` recorded Treasures l `shouldBe` True
      l.cardResources `shouldBe` 0

  describe "treasure before the game" do
    it "lets a discovered treasure go into a player's deck, whoever discovered it" . campaignTestIn fireInTheNight (record Treasures [toCardCode Attachments.jewelsOfWilderland] emLog) testDeck $ \pid -> do
      run $ DoStep 2 Setup
      chooseTarget pid pid
      deck <- (.deck) <$> getPlayer pid
      toCardCode Attachments.jewelsOfWilderland `elem` map toCardCode deck `shouldBe` True

  describe "Fire in the Night" do
    it "ignores the deep keyword and lets each player take a non-burden card out of the pool" . campaignTestIn fireInTheNight emLog {campaignLogPool = [PoolCard (toCardCode Locations.twilightHall) Nothing Nothing]} testDeck $ \pid -> do
      run $ CheckWindows []
      getModifiers GameTarget >>= (`shouldSatisfy` elem IgnoreDeep)
      run $ GameOver Won
      chooseLabel "$label.eredMithrin.allyDeclined" pid
      chooseOptionMatching "remove Twilight Hall" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      l <- getCampaignLog
      poolCodes l `shouldBe` []
      Map.member BeldisCare l.notes `shouldBe` True

  describe "Mount Gundabad" do
    it "ignores the discover keyword; takes A Growing Threat out of the pool and, with 6 treasures, adds Dragon-sickness" . campaignTestIn mountGundabad allButJewels {campaignLogPool = [PoolCard "EMC167" Nothing Nothing]} testDeck $ \pid -> do
      run $ CheckWindows []
      getModifiers GameTarget >>= (`shouldSatisfy` elem IgnoreDiscover)
      run $ GameOver Won
      chooseLabel "$label.pass" pid
      poolCodes <$> getCampaignLog `shouldReturn` ["EMC194"]

  describe "The Fate of Wilderland" do
    it "ignores the deep and discover keywords, and ends the campaign once won" . campaignTestIn theFateOfWilderland (wonFirst ["journey-up-the-anduin", "lost-in-mirkwood", "the-kings-quest", "the-withered-heath", "roam-across-rhovanion", "fire-in-the-night", "the-ghost-of-framsburg", "mount-gundabad"] emLog) testDeck $ \_ -> do
      run $ CheckWindows []
      mods <- getModifiers GameTarget
      (IgnoreDeep `elem` mods, IgnoreDiscover `elem` mods) `shouldBe` (True, True)
      run $ GameOver Won
      nextScenario <$> getCampaignLog `shouldReturn` Nothing
