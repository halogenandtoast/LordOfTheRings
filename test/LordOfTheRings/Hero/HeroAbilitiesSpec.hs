module LordOfTheRings.Hero.HeroAbilitiesSpec (spec) where

import LordOfTheRings.Phase
import TestImport

spec :: Spec
spec = do
  describe "Aragorn" do
    it "may spend 1 resource to ready after committing to the quest" . gameTestWith starterDeck $ \pid -> do
      aragorn <- getHeroWithCode "01001"
      run $ PlaceTokens GameSource (toTarget aragorn) Resource 1
      run $ BeginPhase QuestPhase
      chooseTarget pid aragorn
      chooseAbility pid
      aragorn' <- getHeroWithCode "01001"
      heroExhausted (toAttrs aragorn') `shouldBe` False
      aragorn'.resources `shouldBe` 0

    it "cannot use the response without a resource" . gameTestWith starterDeck $ \pid -> do
      aragorn <- getHeroWithCode "01001"
      run $ BeginPhase QuestPhase
      chooseTarget pid aragorn
      Just question <- getQuestion pid
      [() | AbilityLabel {} <- questionChoices question] `shouldBe` []

  describe "Glóin" do
    it "gains a resource for each point of damage he suffers" . gameTestWith starterDeck $ \pid -> do
      gloin <- getHeroWithCode "01003"
      run $ DealDamage GameSource (toTarget gloin) 2
      chooseAbility pid
      (.resources) <$> getHeroWithCode "01003" `shouldReturn` 2
