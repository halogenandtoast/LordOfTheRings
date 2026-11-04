module LordOfTheRings.EredMithrin.RoamAcrossRhovanionPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyGuardingACard), select)
import LordOfTheRings.Phase (Phase (PlanningPhase, TravelPhase))
import LordOfTheRings.Scenario.Types (encounterDeckL)
import LordOfTheRings.Window (WindowType (EnemyDestroyedByAttack), mkAfter)
import TestImport
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))

haldanCode, eowynCode, theodredCode, beravorCode, legolasCode :: CardCode
haldanCode = "19026"
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"
legolasCode = "01005"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- enemyIds
  run . AddToStaging =<< genEncounterCard def
  fromJustNote "no enemy" . find (`notElem` existing) <$> enemyIds

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  existing <- getsGame (Map.keys . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . find (`notElem` existing) . Map.keys . (.entities.locations))

placementOf :: LocationId -> GameT (Maybe Placement)
placementOf lid = getsGame (fmap (.placement) . Map.lookup lid . (.entities.locations))

allyWithCard :: CardId -> GameT (Maybe Target)
allyWithCard cid = getsGame (fmap toTarget . find ((== cid) . allyCardId . toAttrs) . toList . (.entities.allies))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  AttachmentTarget aid -> maybe False (attachmentExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.attachments)
  _ -> pure False

attachmentsWithCode :: CardDef -> GameT [Placement]
attachmentsWithCode def = getsGame (map (.placement) . filter ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

spec :: Spec
spec = describe "The Roam Across Rhovanion player cards" do
  describe "Haldan" do
    let deck = deckList [haldanCode, eowynCode, theodredCode] testDeck.cards
    it "does not exhaust to quest while the active location has an attachment" . gameTestWith deck $ \pid -> do
      haldan <- hero haldanCode
      lid <- stagedLocation Locations.oldForestRoad
      run $ PlaceLocation lid ActiveLocation
      run $ CommitToQuest haldan
      isExhausted haldan `shouldReturn` True
      run $ Ready haldan
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      run $ CommitToQuest haldan
      isExhausted haldan `shouldReturn` False

    it "draws a card for each attachment on the active location as it is explored" . gameTestWith deck $ \pid -> do
      haldan <- hero haldanCode
      lid <- stagedLocation Locations.forestGate
      run $ PlaceLocation lid ActiveLocation
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      hand <- handSize pid
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
      chooseAbilityOf pid haldan
      handSize pid `shouldReturn` hand + 2

  describe "Wiglaf" do
    it "plays an Item on himself for no cost as he is played" . gameTest $ \pid -> do
      armor <- putCardIntoHand pid Attachments.ancestralArmor
      _ <- playWith pid theodredCode 5 Allies.wiglaf
      wiglaf <- getsGame (toTarget . fromJustNote "not in play" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid wiglaf
      chooseTarget pid (CardIdTarget armor.id)
      attachmentsWithCode Attachments.ancestralArmor `shouldReturn` [AttachedTo wiglaf]
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 0
      getDefense wiglaf `shouldReturn` 3

    it "exhausts an attachment on him to ready him, once a phase" . gameTest $ \pid -> do
      wiglaf <- putAllyIntoPlay pid Allies.wiglaf
      armor <- putAttachmentOn pid Attachments.ancestralArmor wiglaf
      run $ Exhaust wiglaf
      run $ ActionWindow pid
      chooseAbilityOf pid wiglaf
      isExhausted wiglaf `shouldReturn` False
      isExhausted armor `shouldReturn` True
      passActions pid
      run $ Ready armor
      run $ Exhaust wiglaf
      run $ ActionWindow pid
      offersAbilityOf pid wiglaf `shouldReturn` False

  describe "Ancestral Armor" do
    it "gives +2 defense and +2 hit points" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      _ <- putAttachmentOn pid Attachments.ancestralArmor theodred
      getDefense theodred `shouldReturn` 3
      getHitPoints theodred `shouldReturn` 6

  describe "Glamdring" do
    it "is guarded as it is played" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      _ <- playWith pid beravorCode 1 Attachments.glamdring
      length <$> select EnemyGuardingACard `shouldReturn` 1

    it "gives +2 attack, and draws a card after its hero destroys an enemy" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.glamdring beravor
      getAttack beravor `shouldReturn` 4
      eid <- stagedEnemy Enemies.forestSpider
      hand <- handSize pid
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [beravor] mempty]
      chooseAbilityOf pid =<< getsGame (toTarget . fromJustNote "no Glamdring" . headMay . toList . (.entities.attachments))
      handSize pid `shouldReturn` hand + 1

  describe "Woodmen's Path" do
    it "reduces its location's quest points to 1 while it is the active location" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.forestGate
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      getLocationQuestPoints lid `shouldReturn` 4
      run $ PlaceLocation lid ActiveLocation
      getLocationQuestPoints lid `shouldReturn` 1

    it "attaches only to a location with no progress on it" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.forestGate
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
      cardHosts pid (toCardCode Attachments.woodmensPath) `shouldReturn` []

  describe "Meneldor" do
    it "places 2 progress on a location as he enters and as he leaves play" . gameTest $ \pid -> do
      gate <- stagedLocation Locations.forestGate
      road <- stagedLocation Locations.oldForestRoad
      meneldor <- putAllyIntoPlay pid Allies.meneldor
      chooseAbilityOf pid meneldor
      chooseTarget pid (LocationTarget gate)
      (.progress) <$> getLocation gate `shouldReturn` 2
      run $ Discard meneldor
      chooseAbilityOf pid meneldor
      chooseTarget pid (LocationTarget road)
      (.progress) <$> getLocation road `shouldReturn` 2

  describe "Flight of the Eagles and Eagle of the North" do
    let deck = deckList [legolasCode, eowynCode, theodredCode] ((toCardCode Allies.eagleOfTheNorth, 3) : testDeck.cards)
    it "sets the Eagles of the North aside out of the deck" . gameTestWith deck $ \pid -> do
      player <- getPlayer pid
      length player.setAsideCards `shouldBe` 3
      filter ((== toCardCode Allies.eagleOfTheNorth) . toCardCode) player.deck `shouldBe` []

    it "returns an Eagle ally to hand to shuffle an Eagle of the North in, then leaves the game" . gameTestWith deck $ \pid -> do
      _ <- setEncounterDeck []
      _ <- putAllyIntoPlay pid Allies.meneldor
      passResponses pid
      card <- playWith pid legolasCode 0 Events.flightOfTheEagles
      passResponses pid
      any ((== toCardCode Allies.meneldor) . toCardCode) . (.hand) <$> getPlayer pid `shouldReturn` True
      Just chosen <- headMay . (.setAsideCards) <$> getPlayer pid
      chooseTarget pid (CardIdTarget chosen.id)
      map (.id) <$> getEncounterDeck `shouldReturn` [chosen.id]
      elem card.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` False

    it "revealed, it surges, joins a player, discards a non-unique staged enemy, and leaves the game as it leaves play" . gameTestWith deck $ \pid -> do
      Just chosen <- headMay . (.setAsideCards) <$> getPlayer pid
      _ <- setEncounterDeck []
      run $ ShuffleIntoEncounterDeck (CardIdTarget chosen.id)
      [eagle] <- getEncounterDeck
      spider <- stagedEnemy Enemies.forestSpider
      road <- genEncounterCard Locations.oldForestRoad
      putGame . (scenarioL %~ overAttrs (encounterDeckL .~ [eagle, road])) =<< getGame
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid (EnemyTarget spider)
      enemyIds `shouldReturn` []
      Just e <- allyWithCard eagle.id
      getsGame (fmap (\a -> allyPlacement (toAttrs a)) . find ((== e) . toTarget) . toList . (.entities.allies))
        `shouldReturn` Just (InPlayArea pid)
      getsGame (length . (.entities.locations)) `shouldReturn` 1
      run $ Discard e
      allyWithCard eagle.id `shouldReturn` Nothing
      elem eagle.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` False

    it "goes to the encounter discard pile as an encounter card would" . gameTestWith deck $ \pid -> do
      Just eagle <- headMay . (.setAsideCards) <$> getPlayer pid
      run $ ShuffleIntoEncounterDeck (CardIdTarget eagle.id)
      [card] <- filter ((== eagle.id) . (.id)) <$> getEncounterDeck
      run $ RemoveFromEncounterDeck card
      run $ AddToEncounterDiscard [card]
      map (.id) <$> getEncounterDiscard `shouldReturn` [eagle.id]
      elem eagle.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` False

  describe "Wild Stallion" do
    it "attaches itself to an ally as a Mount, giving it +1 to each stat" . gameTest $ \pid -> do
      scout <- putAllyIntoPlay pid Allies.snowbournScout
      stallion <- putAllyIntoPlay pid Allies.wildStallion
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid stallion
      getsGame (\g -> case stallion of AllyTarget aid -> Map.member aid g.entities.allies; _ -> True) `shouldReturn` False
      attachmentsWithCode Allies.wildStallion `shouldReturn` [AttachedTo scout]
      getWillpower scout `shouldReturn` 1
      getAttack scout `shouldReturn` 1
      getDefense scout `shouldReturn` 2
      getHitPoints scout `shouldReturn` 2

  describe "South Away!" do
    it "makes a staged location with a player attachment active, and lowers threat by its attachments" . gameTest $ \pid -> do
      old <- stagedLocation Locations.oldForestRoad
      run $ PlaceLocation old ActiveLocation
      lid <- stagedLocation Locations.forestGate
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      card <- putCardIntoHand pid Events.southAway
      putGame . (playersL . ix pid . threatL .~ 30) =<< getGame
      putGame . (phaseL .~ TravelPhase) =<< getGame
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      placementOf lid `shouldReturn` Just ActiveLocation
      placementOf old `shouldReturn` Just StagingArea
      (.threat) <$> getPlayer pid `shouldReturn` 29
