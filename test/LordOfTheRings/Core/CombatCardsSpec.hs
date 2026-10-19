module LordOfTheRings.Core.CombatCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import TestImport

tacticsTest :: (PlayerId -> GameT ()) -> IO ()
tacticsTest = gameTestWith (coreDeck "tactics")

-- | Forest Spider (2 attack, +1 the round it engages) engaged with the player, with a
-- shadow card that does nothing waiting on top of the encounter deck.
engageSpider :: PlayerId -> GameT EnemyId
engageSpider pid = do
  _ <- setEncounterDeck [Enemies.forestSpider]
  run RevealEncounterCard
  eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  _ <- setEncounterDeck [Locations.oldForestRoad]
  pure eid

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . lookup eid . mapToList . (.entities.enemies))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

playFromHand :: PlayerId -> Card -> GameT ()
playFromHand pid card = chooseTarget pid (CardIdTarget card.id)

fund :: CardCode -> Int -> GameT ()
fund code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

spec :: Spec
spec = do
  describe "Gondorian Spearman" do
    it "deals 1 damage to the attacker after being declared as a defender" . tacticsTest $ \pid -> do
      spearman <- putAllyIntoPlay pid Allies.gondorianSpearman
      eid <- engageSpider pid
      run $ BeginPhase CombatPhase
      passActions pid
      chooseTarget pid spearman
      chooseAbilityOf pid spearman
      enemyDamage eid `shouldReturn` 1

  describe "Swift Strike" do
    it "deals 2 damage to the attacker after a defender is declared" . tacticsTest $ \pid -> do
      card <- putCardIntoHand pid Events.swiftStrike
      fund "01004" 2
      eid <- engageSpider pid
      run $ BeginPhase CombatPhase
      passActions pid
      chooseTarget pid =<< hero "01004"
      playFromHand pid card
      enemyDamage eid `shouldReturn` 2

  describe "Forest Snare" do
    it "stops the attached enemy attacking" . tacticsTest $ \pid -> do
      eid <- engageSpider pid
      _ <- putAttachmentOn pid Attachments.forestSnare (toTarget eid)
      run $ BeginPhase CombatPhase
      passActions pid
      Just (QuestionLabel prompt _ _ _) <- getQuestion pid
      prompt `shouldBe` "Declare attackers"

  describe "Feint" do
    it "stops an engaged enemy attacking this phase" . tacticsTest $ \pid -> do
      card <- putCardIntoHand pid Events.feint
      fund "01004" 1
      eid <- engageSpider pid
      run $ BeginPhase CombatPhase
      playFromHand pid card
      chooseTarget pid eid
      passActions pid
      Just (QuestionLabel prompt _ _ _) <- getQuestion pid
      prompt `shouldBe` "Declare attackers"

  describe "Thicket of Spears" do
    it "is paid from 3 different heroes and stops the player's enemies attacking" . tacticsTest $ \pid -> do
      card <- putCardIntoHand pid Events.thicketOfSpears
      for_ ["01004", "01005", "01006"] \code -> fund code 2
      _ <- engageSpider pid
      run $ BeginPhase CombatPhase
      playFromHand pid card
      for_ ["01004", "01005", "01006"] \code -> chooseTarget pid =<< hero code
      resources <- for ["01004", "01005", "01006"] \code -> (.resources) <$> getHeroWithCode code
      resources `shouldBe` [1, 1, 1]
      passActions pid
      Just (QuestionLabel prompt _ _ _) <- getQuestion pid
      prompt `shouldBe` "Declare attackers"

    it "cannot be played with fewer than 3 heroes able to pay" . tacticsTest $ \pid -> do
      card <- putCardIntoHand pid Events.thicketOfSpears
      fund "01004" 3
      _ <- engageSpider pid
      run $ BeginPhase CombatPhase
      question <- getQuestion pid
      [() | Just q <- [question], TargetLabel (CardIdTarget cid) _ <- questionChoices q, cid == card.id] `shouldBe` []

  describe "Stand Together" do
    it "lets the player declare several defenders" . tacticsTest $ \pid -> do
      card <- putCardIntoHand pid Events.standTogether
      _ <- engageSpider pid
      run $ BeginPhase CombatPhase
      playFromHand pid card
      passActions pid
      chooseTarget pid =<< hero "01004"
      chooseTarget pid =<< hero "01006"
      chooseLabel "Done declaring defenders" pid
      -- 3 attack against 2 + 2 defense
      chooseTarget pid =<< hero "01004"
      (getCharacterDamage =<< hero "01004") `shouldReturn` 0
      heroExhausted . toAttrs <$> getHeroWithCode "01006" `shouldReturn` True
