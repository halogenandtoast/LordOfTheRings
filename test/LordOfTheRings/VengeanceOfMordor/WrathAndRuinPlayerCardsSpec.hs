module LordOfTheRings.VengeanceOfMordor.WrathAndRuinPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyGuardingACard), select)
import LordOfTheRings.Modifier (ModifierType (AfterStagingStep))
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Scenario.Types (victoryDisplayL)
import TestImport

thorinCode, frodoCode, eowynCode, theodredCode, beravorCode :: CardCode
thorinCode = "22001"
frodoCode = "21002"
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"

thorinDeck :: DeckList
thorinDeck = deckList [thorinCode, eowynCode, theodredCode] testDeck.cards

-- | A Hobbit hero and a Lore hero to pay for Tom Bombadillo!, with Tom Bombadil set aside.
tomDeck :: DeckList
tomDeck =
  deckList
    [frodoCode, beravorCode, theodredCode]
    ((toCardCode Allies.tomBombadil, 3) : (toCardCode Events.tomBombadillo, 3) : testDeck.cards)

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . fmap enemyId . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  eid <- stagedEnemy def
  run $ EngageEnemy pid eid
  pure eid

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))

enemyInPlay :: EnemyId -> GameT Bool
enemyInPlay eid = getsGame (Map.member eid . (.entities.enemies))

allyWithCard :: CardId -> GameT (Maybe AllyAttrs)
allyWithCard cid = getsGame (find ((== cid) . allyCardId) . map toAttrs . toList . (.entities.allies))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices <$> getQuestion pid

spec :: Spec
spec = describe "Wrath and Ruin player cards" do
  describe "Thorin Stonehelm" do
    it "discards the top card of your deck to deal 1 damage to an engaged enemy as he attacks" . gameTestWith thorinDeck $ \pid -> do
      thorin <- hero thorinCode
      eid <- engagedEnemy pid Enemies.hillTroll
      deck <- length . (.deck) <$> getPlayer pid
      run $ PlayerAttacks pid
      chooseTarget pid thorin
      chooseAbilityOf pid thorin
      enemyDamage eid `shouldReturn` 1
      length . (.deck) <$> getPlayer pid `shouldReturn` deck - 1
      length . (.discard) <$> getPlayer pid `shouldReturn` 1

    it "is not offered with an empty deck" . gameTestWith thorinDeck $ \pid -> do
      thorin <- hero thorinCode
      _ <- engagedEnemy pid Enemies.hillTroll
      putGame . (playersL . ix pid . deckL .~ []) =<< getGame
      run $ PlayerAttacks pid
      chooseTarget pid thorin
      offersAbilityOf pid thorin `shouldReturn` False

  describe "Cautious Halfling" do
    it "draws a card when the top card of the encounter deck is not an enemy" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Locations.oldForestRoad]
      halfling <- putAllyIntoPlay pid Allies.cautiousHalfling
      chooseAbilityOf pid halfling
      chooseLabel "$label.continue" pid
      length . (.hand) <$> getPlayer pid `shouldReturn` 1

    it "draws nothing when it is an enemy" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      halfling <- putAllyIntoPlay pid Allies.cautiousHalfling
      chooseAbilityOf pid halfling
      chooseLabel "$label.continue" pid
      length . (.hand) <$> getPlayer pid `shouldReturn` 0

  describe "Tom Bombadillo! and Tom Bombadil" do
    it "sets Tom Bombadil aside out of the deck" . gameTestWith tomDeck $ \pid -> do
      player <- getPlayer pid
      length player.setAsideCards `shouldBe` 3
      filter ((== toCardCode Allies.tomBombadil) . toCardCode) player.deck `shouldBe` []

    it "goes to the victory display to shuffle a set-aside Tom Bombadil in, and bars another copy" . gameTestWith tomDeck $ \pid -> do
      _ <- setEncounterDeck []
      other <- putCardIntoHand pid Events.tomBombadillo
      _ <- playWith pid beravorCode 2 Events.tomBombadillo
      victoryDisplay `shouldReturn` [toCardCode Events.tomBombadillo]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Allies.tomBombadil]
      offersCard pid other `shouldReturn` False

    it "cannot be played without a Hobbit hero" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Events.tomBombadillo
      give beravorCode 2
      run $ BeginPhase PlanningPhase
      offersCard pid card `shouldReturn` False

    it "revealed in the quest phase, Tom joins a player ready and committed, and leaves with Tom Bombadillo! at the round's end" . gameTestWith tomDeck $ \pid -> do
      _ <- setEncounterDeck []
      Just chosen <- headMay . (.setAsideCards) <$> getPlayer pid
      run $ ShuffleIntoEncounterDeck (CardIdTarget chosen.id)
      [tom] <- getEncounterDeck
      song <- genPlayerCard Events.tomBombadillo (Just pid)
      putGame . (scenarioL %~ overAttrs (victoryDisplayL .~ [PlayerCard song])) . (phaseL .~ QuestPhase) =<< getGame
      run RevealEncounterCard
      resolveRevealed
      Just a <- allyWithCard tom.id
      (allyPlacement a, allyCommitted a, allyExhausted a) `shouldBe` (InPlayArea pid, True, False)
      getWillpower (toTarget a) `shouldReturn` 3
      run EndRound
      allyWithCard tom.id `shouldReturn` Nothing
      victoryDisplay `shouldReturn` []
      elem tom.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` False

  describe "Durin's Axe" do
    it "is guarded by an enemy as it is played" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      run . PlayAttachment pid =<< genPlayerCard Attachments.durinsAxeTactics (Just pid)
      select EnemyGuardingACard >>= (`shouldSatisfy` ((== 1) . length))

    it "gives +2 attack, and damages an engaged enemy after its hero destroys one" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      axe <- putAttachmentOn pid Attachments.durinsAxeTactics beravor
      getAttack beravor `shouldReturn` 4
      orcs <- engagedEnemy pid Enemies.dolGuldurOrcs
      spider <- engagedEnemy pid Enemies.forestSpider
      run $ PlayerAttacks pid
      chooseTarget pid orcs
      chooseTarget pid beravor
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      chooseAbilityOf pid axe
      enemyInPlay orcs `shouldReturn` False
      enemyDamage spider `shouldReturn` 1

  describe "Charge into Battle" do
    it "attacks each non-unique enemy in the staging area as if engaged, after the staging step" . gameTestWith thorinDeck $ \pid -> do
      bats <- stagedEnemy Enemies.blackForestBats
      orcs <- stagedEnemy Enemies.dolGuldurOrcs
      louis <- stagedEnemy Enemies.louis
      card <- putCardIntoHand pid Events.chargeIntoBattle
      give thorinCode 5
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ CreateEffect $ Effect GameSource GameTarget [AfterStagingStep] UntilEndOfPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      question <- getQuestion pid
      [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (EnemyTarget louis)
      chooseTarget pid bats
      -- Théodred's 2 attack against the bats' 0 defense and 2 hit points
      chooseTarget pid =<< hero theodredCode
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      enemyInPlay bats `shouldReturn` False
      -- then the orcs, the one left
      chooseTarget pid =<< hero eowynCode
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      enemyDamage orcs `shouldReturn` 1

  describe "Silver Circlet" do
    it "goes on a Spirit hero and gives it +2 willpower" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      cardHosts pid (toCardCode Attachments.silverCirclet) `shouldReturn` [eowyn]
      _ <- putAttachmentOn pid Attachments.silverCirclet eowyn
      getWillpower eowyn `shouldReturn` 6
