module LordOfTheRings.KeywordSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (deckProblem, loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Attachment (attachmentCriteriaFor, cardHosts)
import LordOfTheRings.Helpers.Window (passesCriteria)
import LordOfTheRings.Modifier (ModifierType (MayDeclareMultipleDefenders))
import LordOfTheRings.Phase
import TestImport

-- | A table of two: the test deck, then the given deck.
twoPlayerTest :: DeckList -> (PlayerId -> PlayerId -> GameT ()) -> IO ()
twoPlayerTest other body = do
  seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", other)])
  env <- newGameEnv (newTableIn passageThroughMirkwood seats) (const $ pure ()) 1
  runGameT env do
    order <- getsGame (.playerOrder)
    case order of
      [one, two] -> body one two
      _ -> error "two players"

-- | A Forest Spider (2 attack, 1 defense, 4 hit points) engaged with the player.
engagedSpider :: PlayerId -> GameT EnemyId
engagedSpider pid = do
  _ <- setEncounterDeck [Enemies.forestSpider, Enemies.forestSpider]
  run RevealEncounterCard
  eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

io :: IO () -> IO ()
io x = x

questionTargets :: PlayerId -> GameT [Target]
questionTargets pid = do
  question <- getQuestion pid
  pure [t | Just q <- [question], TargetLabel t _ <- questionChoices q]

heroOf :: PlayerId -> CardCode -> GameT Target
heroOf pid code =
  getsGame (toTarget . fromJustNote "no hero" . find (\h -> toCardCode h == code && h.owner == pid) . toList . (.entities.heroes))

spec :: Spec
spec = describe "Keywords and limits" do
  describe "Restricted" do
    it "makes the controller discard one of three restricted attachments" . gameTest $ \pid -> do
      beravor <- toTarget <$> getHeroWithCode "01012"
      _ <- putAttachmentOn pid Attachments.bladeOfGondolin beravor
      _ <- putAttachmentOn pid Attachments.citadelPlate beravor
      third <- putAttachmentOn pid Attachments.dwarvenAxe beravor
      case third of
        AttachmentTarget aid -> run $ AttachCard aid beravor
        _ -> error "not an attachment"
      length <$> questionTargets pid `shouldReturn` 3
      chooseTarget pid third
      getsGame (length . (.entities.attachments)) `shouldReturn` 2

    it "keeps them off Winged Guardian" . gameTestWith (coreDeck "tactics") $ \pid -> do
      guardian <- putAllyIntoPlay pid Allies.wingedGuardian
      legolas <- toTarget <$> getHeroWithCode "01005"
      hosts <- cardHosts (toCardCode Attachments.warAxe)
      hosts `shouldSatisfy` elem legolas
      hosts `shouldSatisfy` notElem guardian

  it "a card limited to 1 per character goes on no host with a copy" . gameTestWith (coreDeck "tactics") $ \pid -> do
    legolas <- toTarget <$> getHeroWithCode "01005"
    thalin <- toTarget <$> getHeroWithCode "01004"
    _ <- putAttachmentOn pid Attachments.warAxe legolas
    hosts <- cardHosts (toCardCode Attachments.warAxe)
    hosts `shouldSatisfy` notElem legolas
    hosts `shouldSatisfy` elem thalin

  describe "Lembas" do
    it "cannot be played without a Noldor or Silvan hero" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Attachments.lembas
      beravor <- getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget beravor) Resource 3
      run $ BeginPhase PlanningPhase
      questionTargets pid >>= (`shouldSatisfy` notElem (toTarget card))

    it "can be with one" . gameTestWith (fromJustNote "no deck" $ lookup "elves-of-lorien" starterDecks) $ \pid -> do
      PlayerCard pc <- putCardIntoHand pid Attachments.lembas
      passesCriteria (attachmentCriteriaFor pc) `shouldReturn` True

  describe "Sentinel" do
    it "is offered to its controller once the attacked player declares no defender" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      eid <- engagedSpider one
      naith <- putAllyIntoPlay two Allies.defenderOfTheNaith
      legolas <- heroOf two "01005"
      run $ EnemyAttack one eid
      questionTargets one >>= (`shouldSatisfy` notElem naith)
      chooseLabel "$label.takeAttackUndefended" one
      targets <- questionTargets two
      targets `shouldSatisfy` elem naith
      targets `shouldSatisfy` notElem legolas
      chooseTarget two naith
      passActions one
      passActions two
      getCharacterDamage naith >>= (`shouldSatisfy` (> 0))
      (getCharacterDamage =<< heroOf one "01012") `shouldReturn` 0

    it "is not offered once the attacked player declares a defender" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      eid <- engagedSpider one
      naith <- putAllyIntoPlay two Allies.defenderOfTheNaith
      run $ EnemyAttack one eid
      chooseTarget one =<< heroOf one "01012"
      questionTargets two >>= (`shouldSatisfy` notElem naith)

    it "leaves the attack undefended when its controller declines" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      eid <- engagedSpider one
      _ <- putAllyIntoPlay two Allies.defenderOfTheNaith
      run $ EnemyAttack one eid
      chooseLabel "$label.takeAttackUndefended" one
      chooseLabel "$label.doNotDefend" two
      Just (QuestionLabel prompt _ _ _) <- getQuestion one
      prompt `shouldBe` "$label.undefendedDamage"

    it "joins the defenders when the attacked player may declare several" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      eid <- engagedSpider one
      naith <- putAllyIntoPlay two Allies.defenderOfTheNaith
      beravor <- heroOf one "01012"
      run $ CreateEffect $ Effect GameSource (PlayerTarget one) [MayDeclareMultipleDefenders] UntilEndOfPhase
      run $ EnemyAttack one eid
      chooseTarget one beravor
      chooseLabel "$label.doneDeclaringDefenders" one
      chooseTarget two naith
      passActions one
      passActions two
      targets <- questionTargets one
      targets `shouldSatisfy` elem naith
      targets `shouldSatisfy` elem beravor

    it "is gained by Guthlaf while a Gondor hero is in play" . twoPlayerTest (coreDeck "lore") $ \one two -> do
      eid <- engagedSpider one
      guthlaf <- putAllyIntoPlay two Allies.guthlaf
      run $ EnemyAttack one eid
      chooseLabel "$label.takeAttackUndefended" one
      questionTargets two >>= (`shouldSatisfy` elem guthlaf)

  describe "Ranged" do
    it "lets Brand son of Bain's controller join another player's attack, then ready one of theirs" . twoPlayerTest ((coreDeck "tactics") {heroes = ["02072", "01005", "01006"]}) $ \one two -> do
      _ <- engagedSpider one
      brand <- heroOf two "02072"
      beravor <- heroOf one "01012"
      run $ PlayerAttacks one
      questionTargets one >>= (`shouldSatisfy` notElem brand)
      chooseTarget one beravor
      chooseLabel "$label.doneAttacking" one
      targets <- questionTargets two
      targets `shouldSatisfy` elem brand
      targets `shouldSatisfy` notElem beravor
      chooseTarget two brand
      -- Legolas, ranged too, may still join
      chooseLabel "$label.doneAttacking" two
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      chooseAbilityOf two brand
      heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  describe "Deck building" do
    it "passes every starter deck" $ io do
      [name | (name, deck) <- starterDecks, isJust (deckProblem deck)] `shouldBe` []

    it "allows 1 copy of a card limited to 1 per deck" $ io do
      deckProblem (DeckList ["01001"] [("17143", 2)]) `shouldSatisfy` isJust
      deckProblem (DeckList ["01001"] [("17143", 1)]) `shouldBe` Nothing

    it "allows 3 copies of a title" $ io do
      deckProblem (DeckList ["01001"] [("01013", 4)]) `shouldSatisfy` isJust

    it "allows 1 to 3 heroes with distinct titles" $ io do
      deckProblem (DeckList ["01001", "01001"] []) `shouldSatisfy` isJust
      deckProblem (DeckList ["01001", "01002", "01003", "01004"] []) `shouldSatisfy` isJust
