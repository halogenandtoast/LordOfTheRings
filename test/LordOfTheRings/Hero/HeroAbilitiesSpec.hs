module LordOfTheRings.Hero.HeroAbilitiesSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
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

  describe "Beravor" do
    it "exhausts to have a player draw 2 cards, once per round" . gameTest $ \pid -> do
      beravor <- getHeroWithCode "01012"
      hand <- length . (.hand) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid beravor
      length . (.hand) <$> getPlayer pid `shouldReturn` hand + 2
      run $ Ready (toTarget beravor)
      run $ BeginPhase PlanningPhase
      offersAbilityOf pid beravor `shouldReturn` False

  describe "Éowyn" do
    it "discards a card for +1 willpower until the end of the phase, once per round" . gameTest $ \pid -> do
      eowyn <- getHeroWithCode "01007"
      card <- putCardIntoHand pid Events.grimResolve
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid eowyn
      chooseTarget pid (CardIdTarget $ (.id) card)
      getWillpower (toTarget eowyn) `shouldReturn` 5
      offersAbilityOf pid eowyn `shouldReturn` False
      passActions pid
      run $ EndPhase PlanningPhase
      getWillpower (toTarget eowyn) `shouldReturn` 4

  describe "Glorfindel" do
    it "pays 1 resource to heal 1 damage, once per round" . gameTestWith (coreDeck "lore") $ \pid -> do
      glorfindel <- getHeroWithCode "01011"
      denethor <- getHeroWithCode "01010"
      run $ PlaceTokens GameSource (toTarget glorfindel) Resource 2
      run $ PlaceTokens GameSource (toTarget denethor) Damage 2
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid glorfindel
      chooseTarget pid denethor
      getCharacterDamage (toTarget denethor) `shouldReturn` 1
      offersAbilityOf pid glorfindel `shouldReturn` False

  describe "Denethor" do
    it "looks at the top encounter card and may move it to the bottom" . gameTestWith (coreDeck "lore") $ \pid -> do
      denethor <- getHeroWithCode "01010"
      [top, second] <- setEncounterDeck [Enemies.forestSpider, Enemies.kingSpider]
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid denethor
      chooseLabel "Move it to the bottom" pid
      getEncounterDeck `shouldReturn` [second, top]

  describe "Gimli" do
    it "gets +1 attack for each damage on him" . gameTestWith (coreDeck "tactics") $ \_ -> do
      gimli <- getHeroWithCode "01004"
      run $ PlaceTokens GameSource (toTarget gimli) Damage 2
      getAttack (toTarget gimli) `shouldReturn` 4
