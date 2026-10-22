module LordOfTheRings.Core.CancelSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

spiritTest :: (PlayerId -> GameT ()) -> IO ()
spiritTest = gameTestWith (coreDeck "spirit")

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

spec :: Spec
spec = do
  describe "A Test of Will" do
    it "cancels the when revealed effects of a revealed card" . spiritTest $ \pid -> do
      card <- putCardIntoHand pid Events.aTestOfWill
      eowyn <- hero "01007"
      run $ PlaceTokens GameSource eowyn Resource 1
      dunhere <- hero "01009"
      run $ Exhaust dunhere
      _ <- setEncounterDeck [Treacheries.theNecromancersReach]
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget card.id)
      getCharacterDamage dunhere `shouldReturn` 0

    it "isn't offered for a card without when revealed effects" . spiritTest $ \pid -> do
      card <- putCardIntoHand pid Events.aTestOfWill
      eowyn <- hero "01007"
      run $ PlaceTokens GameSource eowyn Resource 1
      _ <- setEncounterDeck [Locations.forestGate]
      run RevealEncounterCard
      question <- getQuestion pid
      [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (CardIdTarget card.id)

  describe "Eleanor" do
    it "exhausts to cancel a treachery, discard it and reveal the next card" . spiritTest $ \pid -> do
      dunhere <- hero "01009"
      run $ Exhaust dunhere
      [reach, _] <- setEncounterDeck [Treacheries.theNecromancersReach, Locations.forestGate]
      run RevealEncounterCard
      chooseAbilityOf pid =<< hero "01008"
      getCharacterDamage dunhere `shouldReturn` 0
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode reach]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

  describe "Hasty Stroke" do
    it "cancels a shadow effect as it triggers" . spiritTest $ \pid -> do
      card <- putCardIntoHand pid Events.hastyStroke
      eowyn <- hero "01007"
      run $ PlaceTokens GameSource eowyn Resource 1
      _ <- setEncounterDeck [Enemies.forestSpider]
      run RevealEncounterCard
      eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Enemies.kingSpider]
      run $ BeginPhase CombatPhase
      passActions pid
      chooseTarget pid =<< hero "01008"
      passActions pid
      chooseTarget pid (CardIdTarget card.id)
      -- King Spider's shadow would have the defending player exhaust a character
      Just (QuestionLabel prompt _ _ _) <- getQuestion pid
      prompt `shouldBe` "$label.declareAttackers"
