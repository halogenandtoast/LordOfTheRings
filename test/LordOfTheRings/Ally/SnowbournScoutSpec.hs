module LordOfTheRings.Ally.SnowbournScoutSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Location (Location)
import LordOfTheRings.Phase
import TestImport

playScout :: PlayerId -> GameT Location
playScout pid = do
  flipFirstQuest pid
  card <- putCardIntoHand pid Allies.snowbournScout
  theodred <- getHeroWithCode "01002"
  run $ PlaceTokens GameSource (toTarget theodred) Resource 1
  run $ BeginPhase PlanningPhase
  chooseTarget pid card
  getsGame (fromJustNote "no location" . headMay . toList . (.entities.locations))

progressOn :: Location -> GameT Int
progressOn location = getsGame ((.progress) . fromJustNote "gone" . Map.lookup (toId location) . (.entities.locations))

spec :: Spec
spec = describe "Snowbourn Scout" do
  it "may place 1 progress on a location after entering play" . gameTest $ \pid -> do
    location <- playScout pid
    chooseAbility pid
    progressOn location `shouldReturn` 1

  it "can pass on the response" . gameTest $ \pid -> do
    location <- playScout pid
    chooseLabel "Pass" pid
    progressOn location `shouldReturn` 0
