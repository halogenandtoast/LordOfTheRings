module LordOfTheRings.KeywordSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (deckProblem, loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Attachment (attachmentCriteriaFor, cardHosts)
import LordOfTheRings.Helpers.Window (passesCriteria)
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
      gimli <- toTarget <$> getHeroWithCode "01005"
      hosts <- cardHosts (toCardCode Attachments.warAxe)
      hosts `shouldSatisfy` elem gimli
      hosts `shouldSatisfy` notElem guardian

  it "a card limited to 1 per character goes on no host with a copy" . gameTestWith (coreDeck "tactics") $ \pid -> do
    gimli <- toTarget <$> getHeroWithCode "01005"
    legolas <- toTarget <$> getHeroWithCode "01004"
    _ <- putAttachmentOn pid Attachments.warAxe gimli
    hosts <- cardHosts (toCardCode Attachments.warAxe)
    hosts `shouldSatisfy` notElem gimli
    hosts `shouldSatisfy` elem legolas

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
    it "lets another player's character defend" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      eid <- engagedSpider one
      naith <- putAllyIntoPlay two Allies.defenderOfTheNaith
      gimli <- heroOf two "01005"
      run $ EnemyAttack one eid
      targets <- questionTargets one
      targets `shouldSatisfy` elem naith
      targets `shouldSatisfy` notElem gimli

    it "is gained by Guthlaf while a Gondor hero is in play" . twoPlayerTest (coreDeck "lore") $ \one two -> do
      eid <- engagedSpider one
      guthlaf <- putAllyIntoPlay two Allies.guthlaf
      run $ EnemyAttack one eid
      questionTargets one >>= (`shouldSatisfy` elem guthlaf)

  describe "Ranged" do
    it "lets Brand son of Bain attack for another player, then ready one of theirs" . twoPlayerTest ((coreDeck "tactics") {heroes = ["02072", "01005", "01006"]}) $ \one two -> do
      _ <- engagedSpider one
      brand <- heroOf two "02072"
      beravor <- heroOf one "01012"
      run $ PlayerAttacks one
      questionTargets one >>= (`shouldSatisfy` elem brand)
      chooseTarget one brand
      chooseTarget one beravor
      chooseLabel "$label.doneAttacking" one
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
