module LordOfTheRings.VengeanceOfMordor.TheOneRingSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (barredHeroes, loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEliminationLevel)
import LordOfTheRings.Message.Lifted (discard)
import LordOfTheRings.Modifier (ModifierType (CannotAttackPlayer))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (CommittedToQuest, DeclaredAsAttacker, RoundBegins), mkAfter, mkWhen)
import TestImport

beravorCode, eowynCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"

check :: IO () -> IO ()
check = id

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | The test deck with The One Ring and a Master card in it.
ringDeck :: DeckList
ringDeck = deckList testDeck.heroes (testDeck.cards <> [(toCardCode Attachments.theOneRing, 1), (toCardCode Events.theRingOfPower, 1)])

-- | The One Ring on Beravor, as its Setup would put it.
withRing :: PlayerId -> GameT (Target, Target)
withRing pid = do
  beravor <- hero beravorCode
  ring <- putAttachmentOn pid Attachments.theOneRing beravor
  pure (beravor, ring)

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AttachmentTarget aid -> maybe False (attachmentExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.attachments)
  _ -> pure False

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

choices :: PlayerId -> GameT [Target]
choices pid = do
  question <- getQuestion pid
  pure [t | Just q <- [question], TargetLabel t _ <- questionChoices q]

attachmentInPlay :: Target -> GameT Bool
attachmentInPlay = \case
  AttachmentTarget aid -> Map.member aid <$> getsGame (.entities.attachments)
  _ -> pure False

engagedSpider :: PlayerId -> GameT EnemyId
engagedSpider pid = do
  _ <- setEncounterDeck [Enemies.forestSpider]
  run RevealEncounterCard
  eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

spec :: Spec
spec = describe "The One Ring" do
  describe "its Setup" do
    it "attaches it to a hero and searches the deck for a Master card" . gameTestWith ringDeck $ \pid -> do
      run Setup
      eowyn <- hero eowynCode
      chooseTarget pid eowyn
      master <- fromJustNote "no Master card" . find ((== toCardCode Events.theRingOfPower) . toCardCode) . (.deck) <$> getPlayer pid
      chooseTarget pid (CardIdTarget master.id)
      chooseLabel "$label.keepHand" pid
      ring <- getsGame (find ((== toCardCode Attachments.theOneRing) . toCardCode) . toList . (.entities.attachments))
      fmap (.placement) ring `shouldBe` Just (AttachedTo eowyn)
      elem master.id . map (.id) . (.hand) <$> getPlayer pid `shouldReturn` True
      any ((== toCardCode Attachments.theOneRing) . toCardCode) . (.deck) <$> getPlayer pid `shouldReturn` False

    it "has the first player choose whose copy goes into play, the others removed" $ do
      seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", ringDeck) :| [("two", ringDeck)])
      env <- newGameEnv (newTableIn passageThroughMirkwood seats) (const $ pure ()) 1
      runGameT env do
        run Setup
        lead <- getLead
        other <- fromJustNote "no other" . find (/= lead) <$> getsGame (.playerOrder)
        chooseTarget lead (PlayerTarget other)
        hosts <- choices other
        chooseTarget other (fromJustNote "no host" $ headMay hosts)
        rings <- getsGame (filter ((== toCardCode Attachments.theOneRing) . toCardCode) . toList . (.entities.attachments))
        map (.controller) (map toAttrs rings) `shouldBe` [other]
        any ((== toCardCode Attachments.theOneRing) . toCardCode) . (.deck) <$> getPlayer lead `shouldReturn` False

  it "lowers its controller's threat elimination level by 5" . gameTest $ \pid -> do
    _ <- withRing pid
    getEliminationLevel pid `shouldReturn` 45

  it "loses the game when it leaves play with its hero" . gameTest $ \pid -> do
    (beravor, _) <- withRing pid
    run $ Discard GameSource beravor
    getsGame (.outcome) `shouldReturn` Just Lost

  it "is not offered to an encounter card's discard" . gameTest $ \pid -> do
    (beravor, ring) <- withRing pid
    other <- putAttachmentOn pid Attachments.bladeOfGondolin beravor
    eid <- engagedSpider pid
    run $ ChooseAndDiscard (EnemySource eid) pid [ring, other]
    choices pid `shouldReturn` [other]

  it "is left in play by a card effect's discard" . gameTest $ \pid -> do
    (_, ring) <- withRing pid
    runQueueT (discard GameSource ring)
    void mainLoop
    attachmentInPlay ring `shouldReturn` True
    getsGame (.outcome) `shouldReturn` Nothing

  describe "Master cards" do
    it "are not playable by a player who does not control The One Ring" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Events.theRingOfPower
      run $ ActionWindow pid
      choices pid >>= (`shouldSatisfy` notElem (CardIdTarget card.id))

    it "cannot be used in the saga expansions' scenarios" $ check do
      let deck = deckList testDeck.heroes (testDeck.cards <> [(toCardCode Attachments.theOneRing, 1), (toCardCode Attachments.innerStrength, 1)])
      barredHeroes "a-shadow-of-the-past" False [deck] `shouldBe` ["The One Ring", "Inner Strength"]
      barredHeroes "we-must-away-ere-break-of-day" False [deck] `shouldBe` ["The One Ring", "Inner Strength"]
      barredHeroes "the-hunt-for-gollum" False [deck] `shouldBe` []

  describe "The Ring of Power" do
    it "exhausts the ring to ready its hero, with +1 willpower, attack and defense" . gameTest $ \pid -> do
      (beravor, ring) <- withRing pid
      run $ Exhaust beravor
      threat0 <- threat pid
      card <- putCardIntoHand pid Events.theRingOfPower
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      isExhausted beravor `shouldReturn` False
      isExhausted ring `shouldReturn` True
      threat pid `shouldReturn` (threat0 + 1)
      getWillpower beravor `shouldReturn` 3
      getAttack beravor `shouldReturn` 3
      getDefense beravor `shouldReturn` 3

    it "is not playable while the ring is exhausted" . gameTest $ \pid -> do
      (_, ring) <- withRing pid
      run $ Exhaust ring
      card <- putCardIntoHand pid Events.theRingOfPower
      run $ ActionWindow pid
      choices pid >>= (`shouldSatisfy` notElem (CardIdTarget card.id))

  describe "The Ruling Ring" do
    it "stops an engaged enemy attacking you this phase" . gameTest $ \pid -> do
      (_, ring) <- withRing pid
      eid <- engagedSpider pid
      card <- putCardIntoHand pid Events.theRulingRing
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem (CannotAttackPlayer pid))
      isExhausted ring `shouldReturn` True

    it "is not playable outside the combat phase" . gameTest $ \pid -> do
      _ <- withRing pid
      _ <- engagedSpider pid
      card <- putCardIntoHand pid Events.theRulingRing
      run $ ActionWindow pid
      choices pid >>= (`shouldSatisfy` notElem (CardIdTarget card.id))

  describe "The Master Ring" do
    it "cancels and discards a revealed card, then reveals the next" . gameTest $ \pid -> do
      (_, ring) <- withRing pid
      card <- putCardIntoHand pid Events.theMasterRing
      eowyn <- hero eowynCode
      [reach, _] <- setEncounterDeck [Treacheries.theNecromancersReach, Locations.forestGate]
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget card.id)
      getCharacterDamage eowyn `shouldReturn` 0
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode reach]
      getsGame (length . (.entities.locations)) `shouldReturn` 1
      isExhausted ring `shouldReturn` True

  describe "Inner Strength" do
    it "gives +1 defense and cancels a shadow effect while its hero defends" . gameTest $ \pid -> do
      (beravor, ring) <- withRing pid
      strength <- putAttachmentOn pid Attachments.innerStrength beravor
      getDefense beravor `shouldReturn` 3
      _ <- engagedSpider pid
      _ <- setEncounterDeck [Enemies.kingSpider]
      run $ BeginPhase CombatPhase
      passActions pid
      chooseTarget pid beravor
      passActions pid
      chooseAbilityOf pid strength
      isExhausted ring `shouldReturn` True
      -- King Spider's shadow would have the defending player exhaust a character
      Just (QuestionLabel prompt _ _ _) <- getQuestion pid
      prompt `shouldBe` "$label.declareAttackers"

  describe "Strength and Courage" do
    it "gives +1 attack, and doubles its hero's printed attack for an attack" . gameTest $ \pid -> do
      (beravor, ring) <- withRing pid
      courage <- putAttachmentOn pid Attachments.strengthAndCourage beravor
      getAttack beravor `shouldReturn` 3
      eid <- engagedSpider pid
      run $ CheckWindows [mkAfter $ DeclaredAsAttacker beravor eid]
      chooseAbilityOf pid courage
      getAttack beravor `shouldReturn` 5
      isExhausted ring `shouldReturn` True

  describe "Power of Command" do
    it "gives +1 willpower, and each other unique character +1 willpower after its hero commits" . gameTest $ \pid -> do
      (beravor, _) <- withRing pid
      command <- putAttachmentOn pid Attachments.powerOfCommand beravor
      getWillpower beravor `shouldReturn` 3
      faramir <- putAllyIntoPlay pid Allies.faramir
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      eowyn <- hero eowynCode
      run $ CheckWindows [mkAfter $ CommittedToQuest beravor]
      chooseAbilityOf pid command
      getWillpower beravor `shouldReturn` 3
      getWillpower eowyn `shouldReturn` 5
      getWillpower faramir `shouldReturn` 3
      getWillpower guard' `shouldReturn` 1

  describe "Well Preserved" do
    it "gives +1 hit point, and heals its hero at the beginning of the round" . gameTest $ \pid -> do
      (beravor, ring) <- withRing pid
      preserved <- putAttachmentOn pid Attachments.wellPreserved beravor
      getHitPoints beravor `shouldReturn` 5
      run $ DealDamage GameSource beravor 3
      run $ CheckWindows [mkWhen RoundBegins]
      chooseAbilityOf pid preserved
      getCharacterDamage beravor `shouldReturn` 0
      isExhausted ring `shouldReturn` True

    it "is offered as the round begins" . gameTest $ \pid -> do
      (beravor, _) <- withRing pid
      preserved <- putAttachmentOn pid Attachments.wellPreserved beravor
      run $ DealDamage GameSource beravor 1
      run BeginRound
      offersAbilityOf pid preserved `shouldReturn` True
