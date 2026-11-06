module LordOfTheRings.DarkOfMirkwood.ExtensionSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Campaign
import TestImport

finishedCore :: CampaignLog
finishedCore = (newCampaignLog "revised-core")
  { campaignLogCompleted = [CompletedScenario "passage-through-mirkwood" 101, CompletedScenario "journey-along-the-anduin" 102, CompletedScenario "escape-from-dol-guldur" 103]
  , campaignLogHeroes = [["01001", "01002", "01012"]]
  , campaignLogFallenHeroes = ["01005"]
  , campaignLogThreatPenalty = 2
  , campaignLogPool = [PoolCard "RC002" Nothing (Just "01001")]
  , campaignLogNotes = Map.singleton Prisoner ["01002"]
  }

pureIt :: String -> IO () -> Spec
pureIt = it

spec :: Spec
spec = describe "Extending an existing Mirkwood Paths campaign" do
  pureIt "changes only the campaign route and continues at The Oath" do
    let extended = fromJustNote "expected the completed route to extend" $ extendMirkwoodPaths finishedCore
    extended `shouldBe` finishedCore {campaignLogCampaign = "mirkwood-paths"}
    nextScenario extended `shouldBe` Just "the-oath"
  pureIt "requires completion of all three Core Set scenarios" do
    extendMirkwoodPaths (newCampaignLog "revised-core") `shouldBe` Nothing
  pureIt "cannot extend the mini campaign or an already extended route" do
    extendMirkwoodPaths finishedCore {campaignLogCampaign = "dark-of-mirkwood"} `shouldBe` Nothing
    extendMirkwoodPaths finishedCore {campaignLogCampaign = "mirkwood-paths"} `shouldBe` Nothing

  pureIt "adds one Ranger Sense per seat to the next campaign without carrying the old log" do
    let source = record RangerSenseNextCampaign ["dark-of-mirkwood-049"] finishedCore
        fresh = newCampaignLog "dark-of-mirkwood"
        rewarded = fromJustNote "expected earned reward" $ carryRangerSenseReward 2 source fresh
    rewarded.pool `shouldBe` [PoolCard "dark-of-mirkwood-049" (Just 0) Nothing, PoolCard "dark-of-mirkwood-049" (Just 1) Nothing]
    rewarded.heroes `shouldBe` []
    seatPoolCards 0 rewarded `shouldBe` ["dark-of-mirkwood-049"]
    seatPoolCards 1 rewarded `shouldBe` ["dark-of-mirkwood-049"]
  pureIt "requires the earned Ranger reward and a completed source campaign" do
    carryRangerSenseReward 1 finishedCore (newCampaignLog "dark-of-mirkwood") `shouldBe` Nothing
    carryRangerSenseReward 1 (record RangerSenseNextCampaign ["dark-of-mirkwood-049"] $ newCampaignLog "dark-of-mirkwood") (newCampaignLog "dark-of-mirkwood") `shouldBe` Nothing

  pureIt "lets each player accept or decline the next-campaign copy" do
    let source = record RangerSenseNextCampaign ["dark-of-mirkwood-049"] finishedCore
        fresh = newCampaignLog "dark-of-mirkwood"
        rewarded = fromJustNote "expected selected seat reward" $ carryRangerSenseRewardFor [1] 2 source fresh
    seatPoolCards 0 rewarded `shouldBe` []
    seatPoolCards 1 rewarded `shouldBe` ["dark-of-mirkwood-049"]
    carryRangerSenseRewardFor [2] 2 source fresh `shouldBe` Nothing
    carryRangerSenseRewardFor [0, 0] 2 source fresh `shouldBe` Nothing
