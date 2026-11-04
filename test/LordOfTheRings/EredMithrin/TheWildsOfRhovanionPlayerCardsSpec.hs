module LordOfTheRings.EredMithrin.TheWildsOfRhovanionPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyGuardingACard), hasKeyword, select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import TestImport

brandCode, bardCode, eowynCode, theodredCode, beravorCode, gimliCode :: CardCode
brandCode = "18001"
bardCode = "18002"
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"
gimliCode = "01004"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

resourcesOf :: CardCode -> GameT Int
resourcesOf code = getHeroResources . toId =<< getHeroWithCode code

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

attachmentPlacement :: CardDef -> GameT (Maybe Placement)
attachmentPlacement def =
  getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

handCodes :: PlayerId -> GameT [CardCode]
handCodes pid = map toCardCode . (.hand) <$> getPlayer pid

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid c =
  maybe False (any (\case TargetLabel (CardIdTarget i) _ -> i == c.id; _ -> False) . questionChoices) <$> getQuestion pid

spec :: Spec
spec = describe "The Wilds of Rhovanion player cards" do
  describe "Brand son of Bain" do
    let deck = deckList [brandCode, eowynCode, theodredCode] testDeck.cards
    it "gives Dale characters with a player attachment +1 willpower, and draws as an attachment goes on a bare Dale character" . gameTestWith deck $ \pid -> do
      brand <- hero brandCode
      getWillpower brand `shouldReturn` 2
      hand <- length . (.hand) <$> getPlayer pid
      _ <- playWith pid brandCode 1 Attachments.hauberkOfMail
      chooseTarget pid brand
      chooseAbilityOf pid brand
      getWillpower brand `shouldReturn` 3
      -- the card drawn
      length . (.hand) <$> getPlayer pid `shouldReturn` (hand + 1)

    it "does not draw for an attachment played on a Dale character that already has one" . gameTestWith deck $ \pid -> do
      brand <- hero brandCode
      _ <- putAttachmentOn pid Attachments.kingOfDale brand
      _ <- playWith pid brandCode 1 Attachments.hauberkOfMail
      chooseTarget pid brand
      offersAbilityOf pid brand `shouldReturn` False

  describe "Bard son of Brand" do
    let deck = deckList [bardCode, beravorCode, theodredCode] testDeck.cards
    it "has the tactics icon while playing an Item, a 0-cost one matched by him alone" . gameTestWith deck $ \pid -> do
      bow <- putCardIntoHand pid Attachments.bowOfYew
      valour <- putCardIntoHand pid Events.valourOfTheNorth
      run $ BeginPhase PlanningPhase
      offersCard pid bow `shouldReturn` True
      offersCard pid valour `shouldReturn` False
      chooseTarget pid (CardIdTarget bow.id)
      bard <- hero bardCode
      chooseTarget pid bard
      attachmentPlacement Attachments.bowOfYew `shouldReturn` Just (AttachedTo bard)

    it "returns each attachment of a character leaving play to its owner's hand" . gameTestWith deck $ \pid -> do
      sentry <- putAllyIntoPlay pid Allies.redwaterSentry
      _ <- putAttachmentOn pid Attachments.hauberkOfMail sentry
      run $ Discard sentry
      chooseAbilityOf pid =<< hero bardCode
      handCodes pid >>= (`shouldSatisfy` elem (toCardCode Attachments.hauberkOfMail))
      attachmentPlacement Attachments.hauberkOfMail `shouldReturn` Nothing
      map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` [toCardCode Allies.redwaterSentry]

  describe "Redwater Sentry" do
    it "costs an Armor 1 less, and gets +1 defense and sentinel while it has one" . gameTest $ \pid -> do
      sentry <- putAllyIntoPlay pid Allies.redwaterSentry
      _ <- playWith pid theodredCode 0 Attachments.hauberkOfMail
      chooseTarget pid sentry
      attachmentPlacement Attachments.hauberkOfMail `shouldReturn` Just (AttachedTo sentry)
      -- 2, +1 for its Armor, +1 from the Hauberk
      getDefense sentry `shouldReturn` 4
      hasKeyword Sentinel sentry Allies.redwaterSentry `shouldReturn` True
      -- the Hauberk's hit point for a sentinel
      getHitPoints sentry `shouldReturn` 4

  describe "North Realm Lookout" do
    it "gets +1 willpower and quests without exhausting while it has an Item" . gameTest $ \pid -> do
      lookout <- putAllyIntoPlay pid Allies.northRealmLookout
      getWillpower lookout `shouldReturn` 1
      _ <- putAttachmentOn pid Attachments.mapOfRhovanion lookout
      getWillpower lookout `shouldReturn` 2
      getModifiers lookout >>= (`shouldSatisfy` elem DoesNotExhaustToCommit)

  describe "Warrior of Dale" do
    it "gets +1 attack and ranged while it has a Weapon" . gameTest $ \pid -> do
      warrior <- putAllyIntoPlay pid Allies.warriorOfDale
      _ <- putAttachmentOn pid Attachments.bowOfYew warrior
      getAttack warrior `shouldReturn` 3
      hasKeyword Ranged warrior Allies.warriorOfDale `shouldReturn` True

  describe "Long Lake Trader" do
    it "exhausts to move an Item from a character you control to another eligible one" . gameTest $ \pid -> do
      trader <- putAllyIntoPlay pid Allies.longLakeTrader
      sentry <- putAllyIntoPlay pid Allies.redwaterSentry
      hauberk <- putAttachmentOn pid Attachments.hauberkOfMail sentry
      run $ ActionWindow pid
      chooseAbilityOf pid trader
      chooseTarget pid hauberk
      -- Théodred is a Warrior
      theodred <- hero theodredCode
      chooseTarget pid theodred
      attachmentPlacement Attachments.hauberkOfMail `shouldReturn` Just (AttachedTo theodred)

  describe "King of Dale" do
    let deck = deckList [brandCode, eowynCode, beravorCode] testDeck.cards
    it "lowers the next Dale ally's cost by the different attachments on its hero, without a resource match" . gameTestWith deck $ \pid -> do
      brand <- hero brandCode
      king <- putAttachmentOn pid Attachments.kingOfDale brand
      _ <- putAttachmentOn pid Attachments.hauberkOfMail brand
      sentry <- putCardIntoHand pid Allies.redwaterSentry
      give eowynCode 1
      run $ BeginPhase PlanningPhase
      offersCard pid sentry `shouldReturn` False
      chooseAbilityOf pid king
      -- 3, less 2 for King of Dale and the Hauberk, paid by Éowyn's spirit resource
      chooseTarget pid (CardIdTarget sentry.id)
      getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.redwaterSentry]
      resourcesOf eowynCode `shouldReturn` 0

  describe "Bow of Yew" do
    it "exhausts to deal 1 damage to the enemy its character is declared to attack" . gameTest $ \pid -> do
      warrior <- putAllyIntoPlay pid Allies.warriorOfDale
      bow <- putAttachmentOn pid Attachments.bowOfYew warrior
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      run $ PlayerAttacks pid
      chooseTarget pid warrior
      chooseAbilityOf pid bow
      getDamage (toTarget eid) `shouldReturn` 1

  describe "Map of Rhovanion" do
    it "places 1 progress on the active location as its character commits" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.mountainsOfMirkwood
      run $ TravelTo lid
      lookout <- putAllyIntoPlay pid Allies.northRealmLookout
      _ <- putAttachmentOn pid Attachments.mapOfRhovanion lookout
      run $ CommitCharacters pid
      chooseTarget pid lookout
      chooseLabel "$label.doneCommitting" pid
      chooseAbilityOf pid =<< getsGame (toTarget . fromJustNote "no map" . headMay . toList . (.entities.attachments))
      (.progress) <$> getLocation lid `shouldReturn` 1

  describe "Necklace of Girion" do
    it "is guarded as it is played" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      _ <- playWith pid theodredCode 1 Attachments.necklaceOfGirion
      length <$> select EnemyGuardingACard `shouldReturn` 1

    it "gives its hero +2 willpower and an additional resource" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.necklaceOfGirion eowyn
      getWillpower eowyn `shouldReturn` 6
      getModifiers eowyn >>= (`shouldSatisfy` elem (CollectsAdditionalResources 1))

  describe "Traffic from Dale" do
    let deck = deckList [brandCode, eowynCode, beravorCode] testDeck.cards
    it "adds a resource to a Dale hero for each character with a player attachment, once a round" . gameTestWith deck $ \pid -> do
      brand <- hero brandCode
      _ <- putAttachmentOn pid Attachments.hauberkOfMail brand
      sentry <- putAllyIntoPlay pid Allies.redwaterSentry
      _ <- putAttachmentOn pid Attachments.hauberkOfMail sentry
      traffic <- putCardIntoHand pid Events.trafficFromDale
      again <- putCardIntoHand pid Events.trafficFromDale
      give brandCode 2
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget traffic.id)
      resourcesOf brandCode `shouldReturn` 3
      offersCard pid again `shouldReturn` False

  describe "To Arms!" do
    it "readies an ally with a player attachment" . gameTest $ \pid -> do
      sentry <- putAllyIntoPlay pid Allies.redwaterSentry
      _ <- putAttachmentOn pid Attachments.hauberkOfMail sentry
      run $ Exhaust sentry
      toArms <- putCardIntoHand pid Events.toArms
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget toArms.id)
      getsGame (fmap (allyExhausted . toAttrs) . headMay . toList . (.entities.allies)) `shouldReturn` Just False

  describe "Valour of the North" do
    let deck = deckList [gimliCode, eowynCode, beravorCode] testDeck.cards
    it "gives a character declared against an enemy with an attachment +3 attack and +3 defense for the attack" . gameTestWith deck $ \pid -> do
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      _ <- putAttachmentOn pid Attachments.followed (toTarget eid)
      valour <- putCardIntoHand pid Events.valourOfTheNorth
      gimli <- hero gimliCode
      attack <- getAttack gimli
      run $ PlayerAttacks pid
      chooseTarget pid gimli
      chooseTarget pid (CardIdTarget valour.id)
      getAttack gimli `shouldReturn` (attack + 3)

  describe "Bartering" do
    it "returns a ready attachment to hand to lower the next attachment's cost by its cost" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      _ <- putAttachmentOn pid Attachments.hauberkOfMail theodred
      bartering <- putCardIntoHand pid Events.bartering
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget bartering.id)
      attachmentPlacement Attachments.hauberkOfMail `shouldReturn` Nothing
      hauberk <- fromJustNote "not in hand" . find ((== toCardCode Attachments.hauberkOfMail) . toCardCode) . (.hand) <$> getPlayer pid
      -- Théodred has no resources: it is played at no cost, matched by his leadership icon
      chooseTarget pid (CardIdTarget hauberk.id)
      attachmentPlacement Attachments.hauberkOfMail `shouldReturn` Just (AttachedTo theodred)
