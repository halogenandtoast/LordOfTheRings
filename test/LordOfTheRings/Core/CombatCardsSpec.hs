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
      prompt `shouldBe` "$label.declareAttackers"

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
      prompt `shouldBe` "$label.declareAttackers"

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
      prompt `shouldBe` "$label.declareAttackers"

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
      chooseLabel "$label.doneDeclaringDefenders" pid
      -- 3 attack against 2 + 2 defense
      chooseTarget pid =<< hero "01004"
      (getCharacterDamage =<< hero "01004") `shouldReturn` 0
      heroExhausted . toAttrs <$> getHeroWithCode "01006" `shouldReturn` True

  describe "Legolas" do
    it "places 2 progress on the quest after helping destroy an enemy" . tacticsTest $ \pid -> do
      flipFirstQuest pid
      eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ BeginPhase CombatPhase
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      chooseTarget pid =<< hero "01006"
      chooseTarget pid =<< hero "01005"
      chooseTarget pid =<< hero "01004"
      chooseLabel "$label.doneAttacking" pid
      legolas <- hero "01005"
      chooseAbilityOf pid legolas
      (.progress) <$> getActiveQuest `shouldReturn` 2

  describe "Thalin" do
    it "deals 1 damage to each enemy revealed while he is committed" . tacticsTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ BeginPhase QuestPhase
      chooseTarget pid =<< hero "01006"
      chooseLabel "$label.doneCommitting" pid
      eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      enemyDamage eid `shouldReturn` 1

  describe "Dúnhere" do
    it "attacks an enemy in the staging area alone with +1 attack" . gameTestWith (coreDeck "spirit") $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      run RevealEncounterCard
      eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      run $ BeginPhase CombatPhase
      passActions pid
      chooseTarget pid eid
      -- 2 attack + 1 against 1 defense
      enemyDamage eid `shouldReturn` 2

  describe "Blade of Gondolin" do
    it "gives +1 attack against an Orc and progress after destroying it" . tacticsTest $ \pid -> do
      flipFirstQuest pid
      gimli <- hero "01004"
      _ <- putAttachmentOn pid Attachments.bladeOfGondolin gimli
      _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
      run RevealEncounterCard
      passActions pid
      orcs <- getsGame (map toId . filter ((== toCardCode Enemies.dolGuldurOrcs) . toCardCode) . toList . (.entities.enemies))
      for_ orcs \eid -> run $ EngageEnemy pid eid
      run $ PlayerAttacks pid
      chooseTarget pid gimli
      chooseLabel "$label.doneAttacking" pid
      blade <- getsGame (toTarget . fromJustNote "no blade" . headMay . toList . (.entities.attachments))
      chooseAbilityOf pid blade
      (.progress) <$> getActiveQuest `shouldReturn` 1

  describe "Horn of Gondor" do
    it "adds a resource to the attached hero after a character is destroyed" . tacticsTest $ \pid -> do
      gimli <- hero "01004"
      horn <- putAttachmentOn pid Attachments.hornOfGondor gimli
      axehand <- putAllyIntoPlay pid Allies.veteranAxehand
      run $ DealDamage GameSource axehand 2
      chooseAbilityOf pid horn
      (.resources) <$> getHeroWithCode "01004" `shouldReturn` 1

  describe "Quick Strike" do
    it "exhausts a character to attack an engaged enemy at once" . tacticsTest $ \pid -> do
      eid <- engageSpider pid
      _ <- playWith pid "01004" 1 Events.quickStrike
      chooseTarget pid =<< hero "01004"
      chooseTarget pid eid
      enemyDamage eid `shouldReturn` 1

  describe "Rain of Arrows" do
    it "exhausts a ranged character to deal 1 damage to each enemy engaged with a player" . tacticsTest $ \pid -> do
      eid <- engageSpider pid
      _ <- playWith pid "01004" 1 Events.rainOfArrows
      chooseTarget pid =<< hero "01005"
      enemyDamage eid `shouldReturn` 1
      heroExhausted . toAttrs <$> getHeroWithCode "01005" `shouldReturn` True
