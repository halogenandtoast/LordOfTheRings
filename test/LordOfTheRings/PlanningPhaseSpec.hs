module LordOfTheRings.PlanningPhaseSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Matcher (CardMatcher (AnyCard))
import LordOfTheRings.Modifier (ModifierType (GainKeyword, LowerNextCostOf, NextCardGainsDoomed, PlayedACardThisRound))
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

  it "does not offer events" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Events.everVigilant
    theodred <- getHeroWithCode "01002"
    run $ PlaceTokens GameSource (toTarget theodred) Resource 2
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (toTarget card)

  describe "Doomed" do
    let threat pid = (.threat) <$> getPlayer pid

    it "raises each player's threat by what a card gained as it is played" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource (CardIdTarget card.id) [GainKeyword (Doomed 2)] UntilEndOfRound
      theodred <- getHeroWithCode "01002"
      run $ PlaceTokens GameSource (toTarget theodred) Resource 2
      was <- threat pid
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget card.id)
      threat pid `shouldReturn` was + 2

    it "gives the next card played from hand the Doomed of a spent effect, as Gríma's" . gameTest $ \pid -> do
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [LowerNextCostOf AnyCard 1, NextCardGainsDoomed 1] UntilEndOfRound
      was <- threat pid
      _ <- playWith pid "01002" 1 Allies.guardOfTheCitadel
      threat pid `shouldReturn` was + 1
      -- the spent effect is gone; what is left marks the card played
      getsGame (map effectModifiers . filter ((== PlayerTarget pid) . effectTarget) . gameEffects) `shouldReturn` [[PlayedACardThisRound]]
