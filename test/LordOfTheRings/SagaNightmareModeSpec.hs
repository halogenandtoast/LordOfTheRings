module LordOfTheRings.SagaNightmareModeSpec (spec) where

import LordOfTheRings.Scenario (lookupScenario)
import LordOfTheRings.Scenario.Types (GameMode (..), campaignModeFor)
import Test.Hspec qualified as Hspec
import TestImport

supported :: [ScenarioId]
supported =
  [ "we-must-away-ere-break-of-day", "over-the-misty-mountains-grim", "dungeons-deep-and-caverns-dim"
  , "flies-and-spiders", "the-lonely-mountain", "the-battle-of-five-armies"
  , "a-shadow-of-the-past", "a-knife-in-the-dark", "flight-to-the-ford"
  , "the-ring-goes-south", "journey-in-the-dark", "breaking-of-the-fellowship"
  , "the-uruk-hai", "helms-deep", "the-road-to-isengard"
  , "the-passage-of-the-marshes", "journey-to-the-cross-roads", "shelobs-lair"
  ]

standardChapters :: [ScenarioId]
standardChapters =
  [ "the-passing-of-the-grey-company", "the-siege-of-gondor", "the-battle-of-the-pelennor-fields"
  , "the-tower-of-cirith-ungol", "the-black-gate-opens", "mount-doom"
  ]

modeFor :: GameMode -> ScenarioId -> GameMode
modeFor requested sid = campaignModeFor requested $ toAttrs $ fromJustNote "registered saga scenario" $ lookupScenario sid

spec :: Spec
spec = describe "Saga Nightmare campaign modes" do
  it "uses the Nightmare Deck for all eighteen published saga Nightmare scenarios" do
    map (modeFor NightmareMode) supported `Hspec.shouldBe` replicate 18 NightmareMode
  it "uses normal mode for the later saga chapters without published Nightmare Decks" do
    map (modeFor NightmareMode) standardChapters `Hspec.shouldBe` replicate 6 NormalMode
  it "keeps normal and easy campaigns in their chosen modes" do
    for_ [NormalMode, EasyMode] \requested ->
      map (modeFor requested) (supported <> standardChapters) `Hspec.shouldBe` replicate 24 requested
