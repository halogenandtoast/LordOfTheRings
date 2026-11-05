module LordOfTheRings.VengeanceOfMordor.TheFortressOfNurnPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (AttachmentType), cdCardType, toCardDef)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy (EnemyAttrs (enemyTokens))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Character (getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Location (LocationAttrs (locationTokens))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (Attached, CommittedToQuest, QuestedSuccessfully, StagingStepEnds), mkAfter)
import TestImport

aragornCode, haldanCode, eowynCode, theodredCode, sagaFrodoCode :: CardCode
sagaFrodoCode = "141001"
aragornCode = "22137"
haldanCode = "19026"
eowynCode = "01007"
theodredCode = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

aragornDeck, haldanDeck, frodoDeck, frodoLeadershipDeck :: DeckList
frodoDeck = deckList [sagaFrodoCode, aragornCode, eowynCode] testDeck.cards
frodoLeadershipDeck = deckList ["21002", eowynCode, theodredCode] testDeck.cards
aragornDeck = deckList [aragornCode, eowynCode, theodredCode] testDeck.cards
haldanDeck = deckList [haldanCode, eowynCode, theodredCode] testDeck.cards

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

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))

-- | Whether the open question offers the card in hand to be played.
offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel (CardIdTarget cid) _ -> cid == card.id; _ -> False) . fmap questionChoices <$> getQuestion pid

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
spec = describe "The Fortress of Nurn player cards" do
  describe "Aragorn" do
    it "exhausts to lower a staged location's threat by his willpower, placing progress if it falls to 0" . gameTestWith aragornDeck $ \pid -> do
      aragorn <- hero aragornCode
      lid <- stagedLocation Locations.forestGate
      run $ ActionWindow pid
      chooseAbilityOf pid aragorn
      isExhausted aragorn `shouldReturn` True
      getLocationThreat lid `shouldReturn` 0
      getsGame (maybe 0 (countTokens Progress . locationTokens . toAttrs) . Map.lookup lid . (.entities.locations)) `shouldReturn` 1

    it "places no progress on a location left with threat" . gameTestWith aragornDeck $ \pid -> do
      aragorn <- hero aragornCode
      lid <- stagedLocation Locations.mountainsOfMirkwood
      run $ CreateEffect $ Effect GameSource (toTarget lid) [ThreatModifier 2] UntilEndOfRound
      run $ ActionWindow pid
      chooseAbilityOf pid aragorn
      -- 4 threat less his 2 willpower
      getLocationThreat lid `shouldReturn` 2
      getsGame (maybe 0 (countTokens Progress . locationTokens . toAttrs) . Map.lookup lid . (.entities.locations)) `shouldReturn` 0

  describe "Shining Shield" do
    it "gives its Noble hero +1 defense, +2 once your threat is 40" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      base <- getDefense theodred
      _ <- putAttachmentOn pid Attachments.shiningShield theodred
      getDefense theodred `shouldReturn` base + 1
      run $ RaiseThreat pid 40
      getDefense theodred `shouldReturn` base + 2

  describe "Knowledge of the Enemy" do
    it "adds a resource to a hero for each enemy engaged with you, once a round" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      traverse_ (run . EngageEnemy pid <=< stagedEnemy) [Enemies.forestSpider, Enemies.hillTroll]
      card <- putCardIntoHand pid Events.knowledgeOfTheEnemy
      again <- putCardIntoHand pid Events.knowledgeOfTheEnemy
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid eowyn
      (getHeroResources . toId =<< getHeroWithCode eowynCode) `shouldReturn` 2
      offersCard pid again `shouldReturn` False

  describe "Haleth" do
    it "searches the top 5 cards for an attachment after questing successfully" . gameTest $ \pid -> do
      putOnTopOfDeck pid Attachments.sparePipe
      haleth <- putAllyIntoPlay pid Allies.haleth
      run $ CommitToQuest haleth
      let attachments = length . filter ((== AttachmentType) . cdCardType . toCardDef) . (.hand) <$> getPlayer pid
      prior <- attachments
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      chooseAbilityOf pid haleth
      takeFirstSearched pid
      attachments `shouldReturn` prior + 1

  describe "Woodman Lore" do
    it "readies a Woodman character for each location with a player attachment" . gameTestWith haldanDeck $ \pid -> do
      haldan <- hero haldanCode
      lid <- stagedLocation Locations.forestGate
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      run $ Exhaust haldan
      _ <- playWith pid haldanCode 1 Events.woodmanLore
      chooseTarget pid haldan
      isExhausted haldan `shouldReturn` False

    it "readies none without a location with a player attachment" . gameTestWith haldanDeck $ \pid -> do
      haldan <- hero haldanCode
      run $ Exhaust haldan
      run $ PlaceTokens GameSource haldan Resource 1
      card <- putCardIntoHand pid Events.woodmanLore
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      isExhausted haldan `shouldReturn` True

  describe "Westfold Lancer" do
    it "is discarded after questing successfully to deal 2 damage to a non-unique enemy in the staging area" . gameTest $ \pid -> do
      lancer <- putAllyIntoPlay pid Allies.westfoldLancer
      eid <- stagedEnemy Enemies.hillTroll
      run $ CommitToQuest lancer
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      chooseAbilityOf pid lancer
      enemyDamage eid `shouldReturn` 2
      getsGame (length . (.entities.allies)) `shouldReturn` 0

  describe "Tireless Thoroughbred" do
    it "goes on a Warrior and gives it ranged and sentinel" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      _ <- putAttachmentOn pid Attachments.tirelessThoroughbred theodred
      mods <- getModifiers theodred
      mods `shouldSatisfy` elem (GainKeyword Ranged)
      mods `shouldSatisfy` elem (GainKeyword Sentinel)

  describe "Misty Mountain Journeyman" do
    let journeyman engaged = gameTest $ \pid -> do
          eowyn <- hero eowynCode
          run $ PlaceTokens GameSource eowyn Resource 1
          when engaged $ run . EngageEnemy pid =<< stagedEnemy Enemies.forestSpider
          card <- putCardIntoHand pid Allies.mistyMountainJourneyman
          run $ BeginPhase PlanningPhase
          (elem card.id . map (.id) <$> (playableCards =<< getPlayer pid)) `shouldReturn` engaged
    it "cannot be played from hand while you are not engaged with an enemy" $ journeyman False
    it "can be played from hand while you are engaged with an enemy" $ journeyman True

  describe "A Desperate Path" do
    it "resolves the first treachery discarded to ready your questing characters with +1 willpower" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      run $ PlaceTokens GameSource eowyn Resource 2
      base <- getWillpower eowyn
      _ <- setEncounterDeck [Enemies.forestSpider, Treacheries.theNecromancersReach]
      card <- putCardIntoHand pid Events.aDesperatePath
      run $ CommitToQuest eowyn
      run $ Exhaust eowyn
      run $ CheckWindows [mkAfter StagingStepEnds]
      chooseTarget pid (CardIdTarget card.id)
      resolveRevealed
      -- The Necromancer's Reach deals 1 damage to each exhausted character, before she readies
      getCharacterDamage eowyn `shouldReturn` 1
      isExhausted eowyn `shouldReturn` False
      getWillpower eowyn `shouldReturn` base + 1

  describe "Thorongil" do
    it "attaches another version of its hero, whose text and resource icon the hero gains" . gameTestWith aragornDeck $ \pid -> do
      aragorn <- hero aragornCode
      thorongil <- putAttachmentOn pid Attachments.thorongil aragorn
      run $ CheckWindows [mkAfter $ Attached thorongil aragorn mempty]
      chooseAbilityOf pid thorongil
      -- Aragorn (Core Set): Response: After Aragorn commits to a quest, spend 1 resource from
      -- his resource pool to ready him.
      chooseOptionMatching "attach the Core Set Aragorn" pid \case
        TargetLabel (CardIdTarget _) msgs -> any (\case PlaceUnderneath _ [c] -> toCardCode c == "01001"; _ -> False) msgs
        _ -> False
      getModifiers aragorn >>= (`shouldSatisfy` elem (GainResourceIcon Leadership))
      run $ PlaceTokens GameSource aragorn Resource 1
      run $ Exhaust aragorn
      run $ CheckWindows [mkAfter $ CommittedToQuest aragorn]
      chooseAbilityOf pid aragorn
      isExhausted aragorn `shouldReturn` False
      (getHeroResources . toId =<< getHeroWithCode aragornCode) `shouldReturn` 0

    it "is discarded when Helm of Secrecy swaps its hero out: it is no eligible attachment (Caleb Grace, ALeP FAQ 3.374)" . gameTestWith aragornDeck $ \pid -> do
      aragorn <- hero aragornCode
      thorongil <- putAttachmentOn pid Attachments.thorongil aragorn
      run $ CheckWindows [mkAfter $ Attached thorongil aragorn mempty]
      chooseAbilityOf pid thorongil
      chooseOptionMatching "attach the Core Set Aragorn" pid \case
        TargetLabel (CardIdTarget _) msgs -> any (\case PlaceUnderneath _ [c] -> toCardCode c == "01001"; _ -> False) msgs
        _ -> False
      _ <- playWith pid eowynCode 4 Events.helmOfSecrecy
      chooseTarget pid aragorn
      chooseOptionMatching "swap" pid \case
        TargetLabel _ [SwapHero {}] -> True
        _ -> False
      getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` notElem aragornCode)
      getsGame (map toCardCode . toList . (.entities.attachments)) >>= (`shouldSatisfy` notElem (toCardCode Attachments.thorongil))

    it "cannot be attached to a Fellowship hero" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero sagaFrodoCode
      hosts <- cardHosts pid (toCardCode Attachments.thorongil)
      hosts `shouldSatisfy` notElem frodo
      hosts `shouldSatisfy` notNull

    it "offers only the non-Fellowship versions of its hero" . gameTestWith frodoLeadershipDeck $ \pid -> do
      frodo <- hero "21002"
      thorongil <- putAttachmentOn pid Attachments.thorongil frodo
      run $ CheckWindows [mkAfter $ Attached thorongil frodo mempty]
      chooseAbilityOf pid thorongil
      -- one version is left, the Spirit Frodo Baggins, so it is attached without asking
      getModifiers frodo >>= (`shouldSatisfy` elem (GainGameTextOf "02025"))

    it "takes the hero card with it as it leaves play" . gameTestWith aragornDeck $ \pid -> do
      aragorn <- hero aragornCode
      thorongil <- putAttachmentOn pid Attachments.thorongil aragorn
      card <- genPlayerCard (toCardDef ("01001" :: CardCode)) (Just pid)
      run $ PlaceUnderneath thorongil [PlayerCard card]
      getModifiers aragorn >>= (`shouldSatisfy` elem (GainGameTextOf "01001"))
      run $ Discard thorongil
      getModifiers aragorn >>= (`shouldSatisfy` notElem (GainGameTextOf "01001"))
      discard <- map (.id) . (.discard) <$> getPlayer pid
      discard `shouldSatisfy` notElem card.id
