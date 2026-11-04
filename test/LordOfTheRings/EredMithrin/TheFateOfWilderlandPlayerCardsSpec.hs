module LordOfTheRings.EredMithrin.TheFateOfWilderlandPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (PlayerCard)
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyGuardingACard), LocationMatcher (LocationGuardingACard), select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase, RefreshPhase))
import TestImport

radagastCode, beravorCode, bilboLoreCode, bilboCode, eowynCode, theodredCode :: CardCode
radagastCode = "19145"
beravorCode = "01012"
bilboLoreCode = "02001"
bilboCode = "19112"
eowynCode = "01007"
theodredCode = "01002"

radagastDeck :: DeckList
radagastDeck = deckList [radagastCode, eowynCode, theodredCode] testDeck.cards

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . fmap enemyId . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))

onTopOfDeck :: PlayerId -> CardDef -> GameT PlayerCard
onTopOfDeck pid def = do
  card <- genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (PlayerCard card :)) =<< getGame
  pure card

inDiscard :: PlayerId -> CardDef -> GameT PlayerCard
inDiscard pid def = do
  card <- genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . discardL %~ (PlayerCard card :)) =<< getGame
  pure card

handIds, discardIds :: PlayerId -> GameT [CardId]
handIds pid = map (.id) . (.hand) <$> getPlayer pid
discardIds pid = map (.id) . (.discard) <$> getPlayer pid

allyCodes :: GameT [CardCode]
allyCodes = getsGame (map toCardCode . toList . (.entities.allies))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . Map.lookup hid . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  _ -> pure False

attachmentPlacement :: CardDef -> GameT (Maybe Placement)
attachmentPlacement def =
  getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices <$> getQuestion pid

spec :: Spec
spec = describe "The Fate of Wilderland player cards" do
  describe "Radagast" do
    it "pays for a Creature ally of another sphere, then does not exhaust to quest this round" . gameTestWith radagastDeck $ \pid -> do
      radagast <- hero radagastCode
      -- Messenger Raven is Leadership; Radagast is Lore
      _ <- playWith pid radagastCode 1 Allies.messengerRaven
      allyCodes `shouldReturn` [toCardCode Allies.messengerRaven]
      chooseAbilityOf pid radagast
      run $ CommitToQuest radagast
      isExhausted radagast `shouldReturn` False

  describe "Messenger Raven" do
    it "cannot attack, defend, or have attachments" . gameTest $ \pid -> do
      raven <- putAllyIntoPlay pid Allies.messengerRaven
      getModifiers raven >>= (`shouldSatisfy` \ms -> CannotAttackOrDefend `elem` ms && CannotHaveAttachments `elem` ms)

    it "returns to hand in the refresh phase to take the top card of a deck if it is the named type" . gameTest $ \pid -> do
      raven <- putAllyIntoPlay pid Allies.messengerRaven
      top <- onTopOfDeck pid Allies.loyalHound
      run $ BeginPhase RefreshPhase
      chooseAbilityOf pid raven
      chooseLabel "$label.messengerRaven.ally" pid
      handIds pid >>= (`shouldSatisfy` elem top.id)
      allyCodes `shouldReturn` []
      map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.messengerRaven))

    it "discards the top card if it is not" . gameTest $ \pid -> do
      raven <- putAllyIntoPlay pid Allies.messengerRaven
      top <- onTopOfDeck pid Allies.loyalHound
      run $ BeginPhase RefreshPhase
      chooseAbilityOf pid raven
      chooseLabel "$label.messengerRaven.event" pid
      discardIds pid >>= (`shouldSatisfy` elem top.id)

  describe "The King's Return" do
    it "puts a Guarded attachment from the deck into play, guarded" . gameTestWith (deckList [bilboCode, eowynCode, theodredCode] testDeck.cards) $ \pid -> do
      sting <- onTopOfDeck pid Attachments.sting
      _ <- setEncounterDeck [Enemies.forestSpider]
      give theodredCode 1
      kings <- putCardIntoHand pid Events.theKingsReturn
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget kings.id)
      chooseTarget pid (CardIdTarget sting.id)
      select EnemyGuardingACard >>= (`shouldSatisfy` ((== 1) . length))
      map (.id) . (.deck) <$> getPlayer pid >>= (`shouldSatisfy` notElem sting.id)

  describe "Loyal Hound" do
    it "is discarded to cancel up to 2 damage just dealt to a hero you control" . gameTest $ \pid -> do
      hound <- putAllyIntoPlay pid Allies.loyalHound
      eowyn <- hero eowynCode
      run $ DealDamage GameSource eowyn 3
      chooseAbilityOf pid hound
      getCharacterDamage eowyn `shouldReturn` 1
      allyCodes `shouldReturn` []

  describe "The Great Hunt" do
    let lore = deckList [radagastCode, beravorCode, bilboLoreCode] testDeck.cards
    it "is paid from 3 heroes to discard a non-unique enemy in the staging area" . gameTestWith lore $ \pid -> do
      eid <- stagedEnemy Enemies.forestSpider
      traverse_ (`give` 1) [radagastCode, beravorCode, bilboLoreCode]
      hunt <- putCardIntoHand pid Events.theGreatHunt
      run $ BeginPhase CombatPhase
      chooseTarget pid (CardIdTarget hunt.id)
      traverse_ (chooseTarget pid <=< hero) [radagastCode, beravorCode, bilboLoreCode]
      chooseTarget pid eid
      getsGame (Map.member eid . (.entities.enemies)) `shouldReturn` False

    it "cannot be paid from fewer heroes" . gameTestWith lore $ \pid -> do
      _ <- stagedEnemy Enemies.forestSpider
      give beravorCode 3
      hunt <- putCardIntoHand pid Events.theGreatHunt
      run $ BeginPhase CombatPhase
      offersCard pid hunt `shouldReturn` False

  describe "Bywater Shirriff" do
    it "gets +1 to each stat while you are engaged with an enemy whose engagement cost is above your threat" . gameTest $ \pid -> do
      shirriff <- putAllyIntoPlay pid Allies.bywaterShirriff
      getWillpower shirriff `shouldReturn` 1
      troll <- stagedEnemy Enemies.hillTroll
      run $ EngageEnemy pid troll
      getWillpower shirriff `shouldReturn` 2
      getAttack shirriff `shouldReturn` 2
      getDefense shirriff `shouldReturn` 2

  describe "Gwaihir's Debt" do
    it "cannot be played without a unique Eagle" . gameTestWith (deckList [radagastCode, "01004", theodredCode] testDeck.cards) $ \pid -> do
      give "01004" 1
      debt <- putCardIntoHand pid Events.gwaihirsDebt
      run $ ActionWindow pid
      offersCard pid debt `shouldReturn` False

    it "puts an Eagle ally from the top 5 cards into play, returning it to hand at the end of the round" . gameTestWith (deckList [radagastCode, "01004", theodredCode] testDeck.cards) $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.gwaihir
      vassal <- onTopOfDeck pid Allies.vassalOfTheWindlord
      give "01004" 1
      debt <- putCardIntoHand pid Events.gwaihirsDebt
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget debt.id)
      chooseTarget pid (CardIdTarget vassal.id)
      allyCodes >>= (`shouldSatisfy` elem (toCardCode Allies.vassalOfTheWindlord))
      passActions pid
      run EndRound
      handIds pid >>= (`shouldSatisfy` elem vassal.id)

  describe "Mithril Shirt" do
    it "is guarded by a location as it is played" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run . PlayAttachment pid =<< genPlayerCard Attachments.mithrilShirt (Just pid)
      select LocationGuardingACard >>= (`shouldSatisfy` ((== 1) . length))

    it "reduces damage dealt to its hero by 1" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      shirt <- putAttachmentOn pid Attachments.mithrilShirt eowyn
      run $ DealDamage GameSource eowyn 2
      chooseAbilityOf pid shirt
      getCharacterDamage eowyn `shouldReturn` 1

  describe "Reforged" do
    it "plays an attachment of any sphere from a discard pile for its printed cost in Spirit resources" . gameTest $ \pid -> do
      blade <- inDiscard pid Attachments.bladeOfGondolin
      give eowynCode 1
      reforged <- putCardIntoHand pid Events.reforged
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget reforged.id)
      chooseTarget pid (CardIdTarget blade.id)
      chooseTarget pid =<< hero theodredCode
      theodred <- hero theodredCode
      attachmentPlacement Attachments.bladeOfGondolin `shouldReturn` Just (AttachedTo theodred)
      (getHeroResources . toId =<< getHeroWithCode eowynCode) `shouldReturn` 0
      discardIds pid >>= (`shouldSatisfy` notElem blade.id)

    it "cannot be played with no attachment it can afford" . gameTest $ \pid -> do
      _ <- inDiscard pid Attachments.bladeOfGondolin
      reforged <- putCardIntoHand pid Events.reforged
      run $ BeginPhase PlanningPhase
      offersCard pid reforged `shouldReturn` False

  describe "Radagast's Staff" do
    it "goes on Radagast and readies a Creature ally" . gameTestWith radagastDeck $ \pid -> do
      radagast <- hero radagastCode
      staff <- putAttachmentOn pid Attachments.radagastsStaff radagast
      hound <- putAllyIntoPlay pid Allies.loyalHound
      run $ Exhaust hound
      run $ ActionWindow pid
      chooseAbilityOf pid staff
      chooseLabel "$label.radagastsStaff.ready" pid
      isExhausted hound `shouldReturn` False

    it "makes the next Creature ally cost 2 less" . gameTestWith radagastDeck $ \pid -> do
      staff <- putAttachmentOn pid Attachments.radagastsStaff =<< hero radagastCode
      -- Loyal Hound costs 2, and Radagast has nothing to pay with
      hound <- putCardIntoHand pid Allies.loyalHound
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid staff
      chooseLabel "$label.radagastsStaff.reduce" pid
      chooseTarget pid (CardIdTarget hound.id)
      allyCodes `shouldReturn` [toCardCode Allies.loyalHound]

    it "returns a non-unique Creature enemy engaged with you to the staging area" . gameTestWith radagastDeck $ \pid -> do
      staff <- putAttachmentOn pid Attachments.radagastsStaff =<< hero radagastCode
      eid <- stagedEnemy Enemies.kingSpider
      run $ EngageEnemy pid eid
      run $ ActionWindow pid
      chooseAbilityOf pid staff
      chooseLabel "$label.radagastsStaff.return" pid
      getsGame (fmap (.placement) . Map.lookup eid . (.entities.enemies)) `shouldReturn` Just StagingArea
