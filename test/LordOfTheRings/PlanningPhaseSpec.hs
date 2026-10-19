module LordOfTheRings.PlanningPhaseSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Phase
import TestImport

spec :: Spec
spec = describe "Planning phase" do
  it "plays an ally by spending resources from a hero of its sphere" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Allies.guardOfTheCitadel
    theodred <- getHeroWithCode "01002"
    run $ PlaceTokens GameSource (toTarget theodred) Resource 2
    run $ BeginPhase PlanningPhase
    chooseTarget pid card
    allies <- getsGame (toList . (.entities.allies))
    map toCardCode allies `shouldBe` ["01013"]
    getHeroResources (toId theodred) `shouldReturn` 0
    notElem card . (.hand) <$> getPlayer pid `shouldReturn` True

  it "does not offer cards the player cannot afford" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Allies.guardOfTheCitadel
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    let offered = [t | Just q <- [question], TargetLabel t _ <- questionChoices q]
    offered `shouldSatisfy` notElem (toTarget card)
