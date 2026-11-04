module LordOfTheRings.EredMithrin.TheWitheredHeathPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase, QuestPhase))
import TestImport

grimbeornCode, eowynCode, beravorCode, theodredCode :: CardCode
grimbeornCode = "19001"
eowynCode = "01007"
beravorCode = "01012"
theodredCode = "01002"

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

allyCodes :: GameT [CardCode]
allyCodes = getsGame (map toCardCode . toList . (.entities.allies))

inPhase :: Phase -> GameT ()
inPhase phase = putGame . (phaseL .~ phase) =<< getGame

grimbeornDeck :: DeckList
grimbeornDeck = deckList [grimbeornCode, eowynCode, beravorCode] testDeck.cards

spec :: Spec
spec = describe "The Withered Heath player cards" do
  describe "Grimbeorn the Old" do
    it "spends a resource after defending to attack that enemy, its defense 2 lower" . gameTestWith grimbeornDeck $ \pid -> do
      grimbeorn <- hero grimbeornCode
      give grimbeornCode 1
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid grimbeorn
      passActions pid
      chooseAbilityOf pid grimbeorn
      -- his 3 attack against the spider's 1 defense, less 2
      getDamage (toTarget eid) `shouldReturn` 3
      resourcesOf grimbeornCode `shouldReturn` 0

  describe "Guardian of Esgaroth" do
    it "gets +1 to each stat for each different player attachment, up to +3" . gameTest $ \pid -> do
      guardian <- putAllyIntoPlay pid Allies.guardianOfEsgaroth
      _ <- putAttachmentOn pid Attachments.squiresHelm guardian
      getWillpower guardian `shouldReturn` 2
      getHitPoints guardian `shouldReturn` 4
      traverse_ (\def -> putAttachmentOn pid def guardian) [Attachments.mapOfRhovanion, Attachments.bowOfYew, Attachments.hauberkOfMail]
      getWillpower guardian `shouldReturn` 4
      getAttack guardian `shouldReturn` 4
      -- +3, and the Hauberk's 1
      getDefense guardian `shouldReturn` 5

  describe "Squire's Helm" do
    it "gives its ally +2 hit points" . gameTest $ \pid -> do
      sentry <- putAllyIntoPlay pid Allies.redwaterSentry
      _ <- putAttachmentOn pid Attachments.squiresHelm sentry
      getHitPoints sentry `shouldReturn` 5

  describe "Beorning Skin-changer" do
    it "is discarded in the combat phase to put a Beorning ally of cost 3 or more into play" . gameTest $ \pid -> do
      skin <- putAllyIntoPlay pid Allies.beorningSkinChanger
      guardian <- putCardIntoHand pid Allies.beorningGuardian
      inPhase CombatPhase
      run $ ActionWindow pid
      chooseAbilityOf pid skin
      chooseTarget pid (CardIdTarget guardian.id)
      allyCodes `shouldReturn` [toCardCode Allies.beorningGuardian]
      map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` [toCardCode Allies.beorningSkinChanger]

    it "takes an ally from the discard pile too, but not a cheaper one" . gameTest $ \pid -> do
      skin <- putAllyIntoPlay pid Allies.beorningSkinChanger
      guardian <- PlayerCard <$> genPlayerCard Allies.beorningGuardian (Just pid)
      other <- PlayerCard <$> genPlayerCard Allies.beorningSkinChanger (Just pid)
      putGame . (playersL . ix pid . discardL .~ [other, guardian]) =<< getGame
      inPhase CombatPhase
      run $ ActionWindow pid
      chooseAbilityOf pid skin
      -- the other Skin-changer costs 2
      chooseTarget pid (CardIdTarget guardian.id)
      allyCodes `shouldReturn` [toCardCode Allies.beorningGuardian]

  describe "Beorn's Rage" do
    it "gives an enemy a Beorning character is declared against -2 attack and -2 defense" . gameTestWith grimbeornDeck $ \pid -> do
      give grimbeornCode 1
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      rage <- putCardIntoHand pid Events.beornsRage
      run $ PlayerAttacks pid
      chooseTarget pid =<< hero grimbeornCode
      chooseTarget pid (CardIdTarget rage.id)
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` \ms -> AttackModifier (-2) `elem` ms && DefenseModifier (-2) `elem` ms)

  describe "Woodmen's Clearing" do
    it "has the first player reduce his threat by 3 as its location is explored" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.mountainsOfMirkwood
      run $ TravelTo lid
      clearing <- putAttachmentOn pid Attachments.woodmensClearing (LocationTarget lid)
      run $ RaiseThreat pid 20
      threat <- getThreat pid
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 10
      chooseAbilityOf pid clearing
      getThreat pid `shouldReturn` (threat - 3)

  describe "The Hidden Way" do
    it "makes a location from the top 5 encounter cards active, and staging reveals 1 fewer" . gameTest $ \pid -> do
      old <- stagedLocation Locations.mountainsOfMirkwood
      run $ TravelTo old
      [_, road] <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad]
      way <- putCardIntoHand pid Events.theHiddenWay
      give beravorCode 2
      run $ BeginPhase QuestPhase
      chooseTarget pid (CardIdTarget way.id)
      chooseTarget pid (CardIdTarget road.id)
      getsGame (map (\l -> (toCardCode l, l.placement)) . toList . (.entities.locations))
        >>= (`shouldSatisfy` \ls -> (toCardCode Locations.oldForestRoad, ActiveLocation) `elem` ls && (toCardCode Locations.mountainsOfMirkwood, StagingArea) `elem` ls)
      getModifiers GameTarget >>= (`shouldSatisfy` elem (FewerStagingReveals 1))

  describe "Long Lake Fisherman" do
    it "names a number and takes a card of that printed cost from the top 5 of the deck" . gameTest $ \pid -> do
      cards <- traverse (\def -> PlayerCard <$> genPlayerCard def (Just pid)) [Allies.redwaterSentry, Allies.longLakeTrader, Attachments.bowOfYew]
      putGame . (playersL . ix pid . deckL %~ (cards <>)) =<< getGame
      fisherman <- putAllyIntoPlay pid Allies.longLakeFisherman
      chooseAbilityOf pid fisherman
      chooseLabel "$label.longLakeFisherman.number number=i:3" pid
      let sentry = fromJustNote "no sentry" (headMay cards)
      chooseTarget pid (CardIdTarget sentry.id)
      map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.redwaterSentry))

  describe "Grey Cloak" do
    it "keeps a non-unique enemy in the staging area from engaging you by engagement checks this phase" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      cloak <- putAttachmentOn pid Attachments.greyCloak eowyn
      eid <- stagedEnemy Enemies.forestSpider
      inPhase EncounterPhase
      run $ ActionWindow pid
      chooseAbilityOf pid cloak
      attachmentPlacement Attachments.greyCloak `shouldReturn` Nothing
      run $ EngagementCheck pid 0
      (.placement) <$> getsGame (fromJustNote "no enemy" . Map.lookup eid . (.entities.enemies)) `shouldReturn` StagingArea

  describe "The Arkenstone" do
    it "gives unique characters +1 willpower, and passes to another player's hero, moving 2 threat" . twoPlayerTest testDeck $ \one two -> do
      eowyn <- heroOf one eowynCode
      ark <- putAttachmentOn one Attachments.theArkenstone eowyn
      run $ RaiseThreat one 20
      getWillpower eowyn `shouldReturn` 5
      threatOne <- getThreat one
      threatTwo <- getThreat two
      run $ ActionWindow one
      chooseAbilityOf one ark
      theirs <- heroOf two theodredCode
      chooseTarget one theirs
      attachmentPlacement Attachments.theArkenstone `shouldReturn` Just (AttachedTo theirs)
      getThreat one `shouldReturn` (threatOne - 2)
      getThreat two `shouldReturn` (threatTwo + 2)
      getWillpower eowyn `shouldReturn` 4
