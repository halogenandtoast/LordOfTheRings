module LordOfTheRings.VengeanceOfMordor.TheLandOfSorrowPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (EventType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (AnyCard), EnemyMatcher (EnemyWithEngagementCostAboveThreatOf))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Window (WindowType (EnemyDestroyedByAttack, QuestedSuccessfully, RoundEnds), mkAfter)
import TestImport

gwaihirCode, frodoCode, eowynCode, theodredCode, beravorCode :: CardCode
gwaihirCode = "22108"
frodoCode = "02025"
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

gwaihirDeck, frodoDeck :: DeckList
gwaihirDeck = deckList [gwaihirCode, eowynCode, theodredCode] testDeck.cards
frodoDeck = deckList [frodoCode, eowynCode, theodredCode] testDeck.cards

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  AttachmentTarget aid -> maybe False (attachmentExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.attachments)
  _ -> pure False

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- getsGame (Map.keys . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` existing) . Map.keys . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  existing <- getsGame (Map.keys . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . find (`notElem` existing) . Map.keys . (.entities.locations))

allyCount :: GameT Int
allyCount = getsGame (length . (.entities.allies))

handCodes :: PlayerId -> GameT [CardCode]
handCodes pid = map toCardCode . (.hand) <$> getPlayer pid

attachmentWithCode :: CardDef -> GameT Target
attachmentWithCode def = getsGame (toTarget . fromJustNote "no attachment" . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

-- | Take the first card offered by a search of the deck.
takeFirstSearched :: PlayerId -> GameT ()
takeFirstSearched pid = chooseOptionMatching "take a searched card" pid \case
  TargetLabel (CardIdTarget _) _ -> True
  _ -> False

putOnTopOfDeck :: PlayerId -> CardDef -> GameT ()
putOnTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (card :)) =<< getGame

spec :: Spec
spec = describe "The Land of Sorrow player cards" do
  describe "Gwaihir" do
    it "does not ready in the refresh phase and cannot have restricted attachments" . gameTestWith gwaihirDeck $ \_ -> do
      gwaihir <- hero gwaihirCode
      run $ Exhaust gwaihir
      mods <- getModifiers gwaihir
      mods `shouldSatisfy` elem DoesNotReadyDuringRefresh
      mods `shouldSatisfy` elem CannotHaveRestrictedAttachments

    it "readies after an Eagle ally enters play, once per phase" . gameTestWith gwaihirDeck $ \pid -> do
      gwaihir <- hero gwaihirCode
      run $ Exhaust gwaihir
      _ <- putAllyIntoPlay pid Allies.eagleEmissary
      chooseAbilityOf pid gwaihir
      isExhausted gwaihir `shouldReturn` False
      run $ Exhaust gwaihir
      _ <- putAllyIntoPlay pid Allies.eagleEmissary
      offersAbilityOf pid gwaihir `shouldReturn` False

    it "readies after an Eagle ally leaves play" . gameTestWith gwaihirDeck $ \pid -> do
      gwaihir <- hero gwaihirCode
      emissary <- putAllyIntoPlay pid Allies.eagleEmissary
      passResponses pid
      run $ Exhaust gwaihir
      run $ Discard GameSource emissary
      chooseAbilityOf pid gwaihir
      isExhausted gwaihir `shouldReturn` False

  describe "Hobbit Archer" do
    it "gets +1 attack against an enemy with an engagement cost above your threat" . gameTest $ \pid -> do
      archer <- putAllyIntoPlay pid Allies.hobbitArcher
      getModifiers archer >>= (`shouldSatisfy` elem (AttackBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf pid) 1))

  describe "Red Book of Westmarch" do
    it "gives each Hobbit +1 willpower while its hero quests" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero frodoCode
      _ <- putAttachmentOn pid Attachments.redBookOfWestmarch frodo
      base <- getWillpower frodo
      run $ CommitToQuest frodo
      getWillpower frodo `shouldReturn` base + 1

    it "exhausts after questing successfully to add a resource to its hero" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero frodoCode
      book <- putAttachmentOn pid Attachments.redBookOfWestmarch frodo
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      chooseAbilityOf pid book
      (getHeroResources . toId =<< getHeroWithCode frodoCode) `shouldReturn` 1
      isExhausted book `shouldReturn` True

  describe "Dale Messenger" do
    it "heals 1 damage from each of the chosen player's characters with a player attachment" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      theodred <- hero theodredCode
      _ <- putAttachmentOn pid Attachments.stewardOfGondor eowyn
      run $ PlaceTokens GameSource eowyn Damage 2
      run $ PlaceTokens GameSource theodred Damage 2
      messenger <- putAllyIntoPlay pid Allies.daleMessenger
      chooseAbilityOf pid messenger
      getCharacterDamage eowyn `shouldReturn` 1
      getCharacterDamage theodred `shouldReturn` 2

  describe "Song of Healing" do
    it "discards a card from hand to heal 1 damage from its hero" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      song <- putAttachmentOn pid Attachments.songOfHealing eowyn
      run $ PlaceTokens GameSource eowyn Damage 2
      card <- putCardIntoHand pid Allies.daleMessenger
      run $ ActionWindow pid
      chooseAbilityOf pid song
      getQuestion pid >>= \case
        Just _ -> chooseTarget pid (CardIdTarget card.id)
        Nothing -> pure ()
      getCharacterDamage eowyn `shouldReturn` 1
      handCodes pid >>= (`shouldSatisfy` notElem (toCardCode Allies.daleMessenger))

  describe "Eagle Emissary" do
    it "is discarded after questing successfully with no Tactics resource to pay" . gameTest $ \pid -> do
      emissary <- putAllyIntoPlay pid Allies.eagleEmissary
      getModifiers emissary >>= (`shouldSatisfy` elem CannotHaveRestrictedAttachments)
      run $ CommitToQuest emissary
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      allyCount `shouldReturn` 0

    it "stays when its controller pays 1 Tactics resource" . gameTestWith gwaihirDeck $ \pid -> do
      gwaihir <- hero gwaihirCode
      run $ PlaceTokens GameSource gwaihir Resource 1
      emissary <- putAllyIntoPlay pid Allies.eagleEmissary
      passResponses pid
      run $ CommitToQuest emissary
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      chooseLabel "$label.payResource" pid
      allyCount `shouldReturn` 1
      (getHeroResources . toId =<< getHeroWithCode gwaihirCode) `shouldReturn` 0

  describe "Bilbo's Plan" do
    it "adds the first enemy discarded to the staging area to discard a location with no more threat" . gameTestWith gwaihirDeck $ \pid -> do
      theodred <- hero theodredCode
      eid <- stagedEnemy Enemies.forestSpider
      lid <- stagedLocation Locations.mountainsOfMirkwood
      _ <- setEncounterDeck [Locations.forestGate, Enemies.forestSpider]
      card <- putCardIntoHand pid Events.bilbosPlan
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [theodred] mempty]
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid (LocationTarget lid)
      getsGame (Map.member lid . (.entities.locations)) `shouldReturn` False
      getsGame (Map.size . (.entities.enemies)) `shouldReturn` 2

    it "leaves the enemy in the discard pile with no location it could discard" . gameTestWith gwaihirDeck $ \pid -> do
      theodred <- hero theodredCode
      eid <- stagedEnemy Enemies.forestSpider
      lid <- stagedLocation Locations.forestGate
      _ <- setEncounterDeck [Enemies.hillTroll]
      card <- putCardIntoHand pid Events.bilbosPlan
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [theodred] mempty]
      chooseTarget pid (CardIdTarget card.id)
      -- the Hill Troll's 1 threat is less than the Forest Gate's 2
      getsGame (Map.member lid . (.entities.locations)) `shouldReturn` True
      getsGame (Map.size . (.entities.enemies)) `shouldReturn` 1
      getsGame (map toCardCode . (.encounterDiscard) . toAttrs . (.scenario)) >>= (`shouldSatisfy` elem (toCardCode Enemies.hillTroll))

  describe "Spare Pipe" do
    it "gives +1 hit point, and searches the top 5 cards for an event as it is played" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      base <- getHitPoints eowyn
      putOnTopOfDeck pid Events.smokeAndThink
      _ <- playWith pid eowynCode 1 Attachments.sparePipe
      chooseTarget pid eowyn
      pipe <- attachmentWithCode Attachments.sparePipe
      getHitPoints eowyn `shouldReturn` base + 1
      events <- length . filter ((== EventType) . cdCardType . toCardDef) . (.hand) <$> getPlayer pid
      chooseAbilityOf pid pipe
      takeFirstSearched pid
      length . filter ((== EventType) . cdCardType . toCardDef) . (.hand) <$> getPlayer pid `shouldReturn` events + 1

  describe "Smoke and Think" do
    it "lowers the next card's cost by 1 for each Pipe you control" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.sparePipe eowyn
      _ <- putAttachmentOn pid Attachments.sparePipe beravor
      card <- putCardIntoHand pid Events.smokeAndThink
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (LowerNextCostOf AnyCard 2))

  describe "Wilyador" do
    it "is discarded at the end of the round unless the group spends a resource" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      run $ PlaceTokens GameSource eowyn Resource 1
      wilyador <- putAllyIntoPlay pid Allies.wilyadorTheLandOfSorrow
      getModifiers wilyador >>= (`shouldSatisfy` elem CannotHaveRestrictedAttachments)
      run $ CheckWindows [mkAfter RoundEnds]
      chooseLabel "$label.groupDoNotSpend" pid
      allyCount `shouldReturn` 0

    it "stays when the group spends a resource of any sphere" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      run $ PlaceTokens GameSource eowyn Resource 1
      _ <- putAllyIntoPlay pid Allies.wilyadorTheLandOfSorrow
      run $ CheckWindows [mkAfter RoundEnds]
      chooseLabel "$label.groupSpend" pid
      getQuestion pid >>= \case
        Just _ -> chooseTarget pid eowyn
        Nothing -> pure ()
      allyCount `shouldReturn` 1
      (getHeroResources . toId =<< getHeroWithCode eowynCode) `shouldReturn` 0

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()
