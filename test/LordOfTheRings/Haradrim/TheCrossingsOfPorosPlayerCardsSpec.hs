module LordOfTheRings.Haradrim.TheCrossingsOfPorosPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (cdUnique, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import TestImport

quickbeamCode, frodoCode, eowynCode, theodredCode, glorfindelCode, beravorCode :: CardCode
quickbeamCode = "17135"
frodoCode = "02025"
eowynCode = "01007"
theodredCode = "01002"
glorfindelCode = "01011"
beravorCode = "01012"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

resourcesOf :: CardCode -> GameT Int
resourcesOf code = getHeroResources . toId =<< getHeroWithCode code

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

defeatSideQuest :: PlayerId -> CardDef -> GameT ()
defeatSideQuest pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PutSideQuestIntoPlay pid card
  qid <- getsGame (fromJustNote "no side quest" . fmap toId . find ((== card.id) . questCardId . toAttrs) . toList . (.entities.quests))
  run $ CompleteQuest qid

heroExhaustedOf :: CardCode -> GameT Bool
heroExhaustedOf code = heroExhausted . toAttrs <$> getHeroWithCode code

attachmentPlacement :: CardDef -> GameT (Maybe Placement)
attachmentPlacement def =
  getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

spec :: Spec
spec = describe "The Crossings of Poros player cards" do
  describe "Quickbeam" do
    let deck = deckList [quickbeamCode, frodoCode, eowynCode] testDeck.cards
    it "is unique, as errata'd" . gameTestWith deck $ \_ ->
      cdUnique . toCardDef <$> getHeroWithCode quickbeamCode `shouldReturn` True

    it "takes 2 damage to ready, once a phase" . gameTestWith deck $ \pid -> do
      quickbeam <- hero quickbeamCode
      run $ Exhaust quickbeam
      run $ ActionWindow pid
      chooseAbilityOf pid quickbeam
      heroExhaustedOf quickbeamCode `shouldReturn` False
      getCharacterDamage quickbeam `shouldReturn` 2
      passActions pid
      run $ Exhaust quickbeam
      run $ ActionWindow pid
      offersAbilityOf pid quickbeam `shouldReturn` False

  describe "Halfast Gamgee" do
    it "costs 1 less for each side quest in the victory display, and adds a resource as he enters play" . gameTest $ \pid -> do
      defeatSideQuest pid Quests.keepWatch
      _ <- playWith pid theodredCode 2 Allies.halfastGamgee
      halfast <- getsGame (toTarget . fromJustNote "not in play" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid halfast
      chooseTarget pid =<< hero eowynCode
      resourcesOf theodredCode `shouldReturn` 0
      resourcesOf eowynCode `shouldReturn` 1

  describe "Bulwark of the West" do
    it "discards an ally you control to discard a Condition attachment" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.beorningGuardian
      eid <- stagedEnemy Enemies.forestSpider
      _ <- putAttachmentOn pid Attachments.followed (toTarget eid)
      bulwark <- putCardIntoHand pid Events.bulwarkOfTheWest
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget bulwark.id)
      -- the only ally and the only Condition are taken without asking
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      attachmentPlacement Attachments.followed `shouldReturn` Nothing

  describe "Beorning Guardian" do
    it "is discarded after an attack it joined destroys an enemy, placing its printed threat as progress" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.mountainsOfMirkwood
      guardian <- putAllyIntoPlay pid Allies.beorningGuardian
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      run $ PlaceTokens GameSource (toTarget eid) Damage 3
      run $ PlayerAttacks pid
      chooseTarget pid guardian
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      chooseAbilityOf pid guardian
      (.progress) <$> getLocation lid `shouldReturn` 2
      getsGame (length . (.entities.allies)) `shouldReturn` 0

  describe "Followed" do
    it "waits in your play area, attaches to the next enemy engaging you, and lowers the staging threat by its threat" . gameTest $ \pid -> do
      traverse_ stagedLocation [Locations.mountainsOfMirkwood, Locations.mountainsOfMirkwood]
      -- played, as its cost is paid
      run . PlayAttachment pid =<< genPlayerCard Attachments.followed (Just pid)
      attachmentPlacement Attachments.followed `shouldReturn` Just (InPlayArea pid)
      locations <- getStagingThreat
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      passResponses pid
      attachmentPlacement Attachments.followed `shouldReturn` Just (AttachedTo (toTarget eid))
      -- the locations', less the spider's printed 2
      getStagingThreat `shouldReturn` max 0 (locations - 2)
      locations `shouldSatisfy` (> 2)

  describe "Flight to the Sea" do
    it "needs a Noldor or Silvan hero, and removes itself from the game" . gameTestWith (deckList [glorfindelCode, beravorCode, eowynCode] testDeck.cards) $ \pid -> do
      flight <- putCardIntoHand pid Events.flightToTheSea
      give eowynCode 2
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget flight.id)
      player <- getPlayer pid
      map (.id) (player.hand <> player.discard) `shouldSatisfy` notElem flight.id

  describe "The Dam Bursts" do
    let deck = deckList [quickbeamCode, frodoCode, beravorCode] testDeck.cards
    it "readies each damaged Ent you control" . gameTestWith deck $ \pid -> do
      quickbeam <- hero quickbeamCode
      run $ PlaceTokens GameSource quickbeam Damage 1
      run $ Exhaust quickbeam
      dam <- putCardIntoHand pid Events.theDamBursts
      give beravorCode 2
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget dam.id)
      heroExhaustedOf quickbeamCode `shouldReturn` False
