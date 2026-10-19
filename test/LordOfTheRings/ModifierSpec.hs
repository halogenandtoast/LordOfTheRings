module LordOfTheRings.ModifierSpec (spec) where

import LordOfTheRings.Effect
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Modifier
import LordOfTheRings.Phase
import TestImport

spec :: Spec
spec = describe "Lasting effects" do
  it "modify a stat until the end of the phase" . gameTest $ \_ -> do
    eowyn <- getHeroWithCode "01007"
    getWillpower (toTarget eowyn) `shouldReturn` 4
    run $ CreateEffect $ Effect GameSource (toTarget eowyn) [WillpowerModifier 2] UntilEndOfPhase
    getWillpower (toTarget eowyn) `shouldReturn` 6
    run $ EndPhase QuestPhase
    getWillpower (toTarget eowyn) `shouldReturn` 4

  it "until the end of the round survive the end of a phase" . gameTest $ \_ -> do
    eowyn <- getHeroWithCode "01007"
    run $ CreateEffect $ Effect GameSource (toTarget eowyn) [WillpowerModifier 1] UntilEndOfRound
    run $ EndPhase QuestPhase
    getWillpower (toTarget eowyn) `shouldReturn` 5
    run EndRound
    getWillpower (toTarget eowyn) `shouldReturn` 4
