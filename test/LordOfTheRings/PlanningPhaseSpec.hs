module LordOfTheRings.PlanningPhaseSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Matcher (CardMatcher (AnyCard, CardWithSphere), HeroMatcher (HeroControlledBy), PlayerMatcher (PlayerWithId), select)
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Modifier (ModifierType (CardFromHandThisRound, GainKeyword, GainResourceIconFor, LowerNextCostOf, NextCardGainsDoomed, PlayedACardThisPhase, PlayedCardThisPhase, PlayedCardThisRound, RaisedThreatThisRound))
import LordOfTheRings.Phase
import LordOfTheRings.Sphere (Sphere (Tactics))
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

  it "does not offer a 0 cost card without a hero of its sphere (FAQ 2.39)" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Events.unseenStrike
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (toTarget card)

  it "offers a 0 cost card with a hero of its sphere" . gameTestWith testDeck {heroes = ["01012", "01004", "01002"]} $ \pid -> do
    card <- putCardIntoHand pid Events.unseenStrike
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` elem (toTarget card)

  it "does not offer a 0 cost card through A Good Harvest's named sphere (FAQ 2.61)" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Events.unseenStrike
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes \h -> run $ CreateEffect $ Effect GameSource (toTarget h) [GainResourceIconFor (CardWithSphere Tactics) Tactics] UntilEndOfPhase
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (toTarget card)

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
      -- The spent effect is gone; ignore the separate from-hand quota bookkeeping.
      let quotaMarker e = any (\case CardFromHandThisRound _ -> True; _ -> False) (effectModifiers e)
      getsGame (map effectModifiers . filter (\e -> effectTarget e == PlayerTarget pid && not (quotaMarker e)) . gameEffects) `shouldReturn` [[PlayedACardThisPhase, PlayedCardThisPhase (toCardCode Allies.guardOfTheCitadel) AllyType], [PlayedCardThisRound (toCardCode Allies.guardOfTheCitadel) AllyType], [RaisedThreatThisRound]]

  it "marks a card played at no cost as played this round and this phase" . gameTest $ \pid -> do
    card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
    run $ PlayAtNoCost pid card
    getsGame (map effectModifiers . filter ((== PlayerTarget pid) . effectTarget) . gameEffects) `shouldReturn` [[PlayedACardThisPhase, PlayedCardThisPhase (toCardCode Allies.guardOfTheCitadel) AllyType], [PlayedCardThisRound (toCardCode Allies.guardOfTheCitadel) AllyType]]
