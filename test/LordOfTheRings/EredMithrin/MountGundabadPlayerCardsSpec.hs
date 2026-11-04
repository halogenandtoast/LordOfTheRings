module LordOfTheRings.EredMithrin.MountGundabadPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Character (getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Matcher (EnemyMatcher (EnemyGuardingACard), select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Placement (GuardRelease (..))
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

bilboCode, frodoCode, pippinCode, eowynCode, theodredCode, gimliCode :: CardCode
bilboCode = "19112"
frodoCode = "02025"
pippinCode = "06056"
eowynCode = "01007"
theodredCode = "01002"
gimliCode = "01004"

bilboDeck :: DeckList
bilboDeck = deckList [bilboCode, eowynCode, theodredCode] testDeck.cards

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | An enemy in the staging area, and its card.
stagedEnemy :: CardDef -> GameT (EnemyId, EncounterCard)
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  e <- getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))
  pure (enemyId e, card)

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

handIds :: PlayerId -> GameT [CardId]
handIds pid = map (.id) . (.hand) <$> getPlayer pid

attachmentPlacement :: CardDef -> GameT (Maybe Placement)
attachmentPlacement def =
  getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

-- | Bilbo alone commits to the quest, and his response is taken.
commit :: PlayerId -> Target -> GameT ()
commit pid bilbo = do
  run $ BeginPhase QuestPhase
  chooseTarget pid bilbo
  chooseLabel "$label.doneCommitting" pid
  chooseAbilityOf pid bilbo

spec :: Spec
spec = describe "Mount Gundabad player cards" do
  describe "Bilbo Baggins" do
    it "gets the chosen enemy's threat as willpower, and damages it once the players quest successfully" . gameTestWith bilboDeck $ \pid -> do
      bilbo <- hero bilboCode
      (eid, _) <- stagedEnemy Enemies.forestSpider
      commit pid bilbo
      -- 1, and the spider's 2 threat
      getWillpower bilbo `shouldReturn` 3
      -- 3 willpower against the spider's 2 threat
      passActions pid
      enemyDamage eid `shouldReturn` 1

    it "deals 2 damage instead to an enemy guarding a card" . gameTestWith bilboDeck $ \pid -> do
      bilbo <- hero bilboCode
      (eid, card) <- stagedEnemy Enemies.forestSpider
      held <- genPlayerCard Attachments.stewardOfGondor (Just pid)
      run $ GuardPlayerCard card.id ReturnToHandWhenFree held
      commit pid bilbo
      -- 3 willpower against the spider's 2 threat
      passActions pid
      enemyDamage eid `shouldReturn` 2

    it "still damages the enemy if Bilbo has left play by the time the players quest successfully" . gameTestWith bilboDeck $ \pid -> do
      bilbo <- hero bilboCode
      eowyn <- hero eowynCode
      (eid, _) <- stagedEnemy Enemies.forestSpider
      run $ BeginPhase QuestPhase
      chooseTarget pid bilbo
      chooseTarget pid eowyn
      chooseLabel "$label.doneCommitting" pid
      chooseAbilityOf pid bilbo
      run $ RemoveFromGame bilbo
      -- Éowyn's 4 willpower against the spider's 2 threat
      passActions pid
      enemyDamage eid `shouldReturn` 1

  describe "Greenwood Defender" do
    it "does not exhaust to defend the round it enters play" . gameTest $ \pid -> do
      defender <- putAllyIntoPlay pid Allies.greenwoodDefender
      chooseAbilityOf pid defender
      getModifiers defender >>= (`shouldSatisfy` elem DoesNotExhaustToDefend)

  describe "Elf Guide" do
    it "returns a Silvan ally to hand to add a resource to a hero" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.greenwoodDefender
      passResponses pid
      guide <- putCardIntoHand pid Events.elfGuide
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget guide.id)
      chooseTarget pid =<< hero eowynCode
      (getHeroResources . toId =<< getHeroWithCode eowynCode) `shouldReturn` 1
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      handIds pid >>= (`shouldSatisfy` notElem guide.id)
      length <$> handIds pid `shouldReturn` 1

  describe "Gaffer Gamgee" do
    it "returns to hand so an enemy with engagement cost above your threat cannot attack you" . gameTest $ \pid -> do
      run $ RaiseThreat pid 27
      gaffer <- putAllyIntoPlay pid Allies.gafferGamgee
      (troll, _) <- stagedEnemy Enemies.hillTroll
      _ <- stagedEnemy Enemies.eastBightPatrol
      run $ BeginPhase CombatPhase
      chooseAbilityOf pid gaffer
      -- the patrol's engagement cost is below the threat, so the troll is taken
      getModifiers troll >>= (`shouldSatisfy` elem (CannotAttackPlayer pid))
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.gafferGamgee))

  describe "Drinking Song" do
    let sing deck extra = gameTestWith deck $ \pid -> do
          traverse_ (putCardIntoHand pid) [Allies.greenwoodDefender, Allies.gafferGamgee]
          inHand <- handIds pid
          song <- putCardIntoHand pid Events.drinkingSong
          run $ ActionWindow pid
          chooseTarget pid (CardIdTarget song.id)
          drawn <- handIds pid
          length drawn `shouldBe` length inHand + extra
          drawn `shouldSatisfy` all (`notElem` inHand)
    it "shuffles the hand away for as many new cards" $ sing (deckList [eowynCode, theodredCode, "01012"] testDeck.cards) 0
    it "draws 1 more for a unique Hobbit" $ sing (deckList [bilboCode, "01012", theodredCode] testDeck.cards) 1
    it "shuffles an encounter card held in hand into the deck with the rest" . gameTest $ \pid -> do
      _ <- putCardIntoHand pid Allies.greenwoodDefender
      lost <- genEncounterCard Treacheries.lostInTheWild
      putGame . (playersL . ix pid . handL <>~ [EncounterCard lost]) =<< getGame
      song <- putCardIntoHand pid Events.drinkingSong
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget song.id)
      hand <- map (.id) . (.hand) <$> getPlayer pid
      length hand `shouldBe` 2
      deck <- map (.id) . (.deck) <$> getPlayer pid
      (lost.id `elem` deck || lost.id `elem` hand) `shouldBe` True

  describe "Sting" do
    it "is guarded by an enemy as it is played" . gameTestWith bilboDeck $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      run . PlayAttachment pid =<< genPlayerCard Attachments.sting (Just pid)
      select EnemyGuardingACard >>= (`shouldSatisfy` ((== 1) . length))
      attachmentPlacement Attachments.sting `shouldReturn` Nothing

    it "goes on a Hobbit hero, gives +1 to each stat, and damages the enemy it defends against" . gameTestWith bilboDeck $ \pid -> do
      bilbo <- hero bilboCode
      cardHosts pid (toCardCode Attachments.sting) `shouldReturn` [bilbo]
      sting <- putAttachmentOn pid Attachments.sting bilbo
      getWillpower bilbo `shouldReturn` 2
      getDefense bilbo `shouldReturn` 3
      (eid, _) <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid bilbo
      chooseAbilityOf pid sting
      enemyDamage eid `shouldReturn` 1

  describe "Round Shield" do
    it "exhausts after a shadow effect for +2 defense this attack" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      shield <- putAttachmentOn pid Attachments.roundShield eowyn
      (eid, card) <- stagedEnemy Enemies.forestSpider
      base <- getDefense eowyn
      run $ CheckWindows [mkAfter $ ShadowEffectResolved card.id eid]
      chooseAbilityOf pid shield
      getDefense eowyn `shouldReturn` base + 2
      getsGame (fmap (attachmentExhausted . toAttrs) . find ((== shield) . toTarget) . toList . (.entities.attachments))
        `shouldReturn` Just True

  describe "Erebor Toymaker" do
    it "played from hand, makes the next attachment cost 2 less" . gameTest $ \pid -> do
      -- Unexpected Courage costs 2: nothing is left to pay with once the Toymaker is paid for
      courage <- putCardIntoHand pid Attachments.unexpectedCourage
      _ <- playWith pid eowynCode 3 Allies.ereborToymaker
      toymaker <- getsGame (toTarget . fromJustNote "no ally" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid toymaker
      chooseTarget pid (CardIdTarget courage.id)
      chooseTarget pid =<< hero eowynCode
      attachmentPlacement Attachments.unexpectedCourage >>= (`shouldSatisfy` isJust)

  describe "Armor of Erebor" do
    it "goes on a Dale or Dwarf character, which gets +1 defense and sentinel" . gameTestWith (deckList [gimliCode, eowynCode, theodredCode] testDeck.cards) $ \pid -> do
      gimli <- hero gimliCode
      cardHosts pid (toCardCode Attachments.armorOfErebor) `shouldReturn` [gimli]
      base <- getDefense gimli
      _ <- putAttachmentOn pid Attachments.armorOfErebor gimli
      getDefense gimli `shouldReturn` base + 1
      getModifiers gimli >>= (`shouldSatisfy` elem (GainKeyword Sentinel))

  describe "The Shirefolk" do
    it "reduces your threat by 4 when each of your heroes is a Hobbit" . gameTestWith (deckList [bilboCode, frodoCode, pippinCode] testDeck.cards) $ \pid -> do
      run $ RaiseThreat pid 22
      threat <- (.threat) <$> getPlayer pid
      shirefolk <- putCardIntoHand pid Events.theShirefolk
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget shirefolk.id)
      (.threat) <$> getPlayer pid `shouldReturn` threat - 4

    it "cannot be played otherwise" . gameTestWith bilboDeck $ \pid -> do
      shirefolk <- putCardIntoHand pid Events.theShirefolk
      run $ ActionWindow pid
      q <- getQuestion pid
      maybe False (any \case TargetLabel t _ -> t == CardIdTarget shirefolk.id; _ -> False) (questionChoices <$> q) `shouldBe` False
