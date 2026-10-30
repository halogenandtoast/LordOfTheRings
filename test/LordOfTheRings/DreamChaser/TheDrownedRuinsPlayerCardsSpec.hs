module LordOfTheRings.DreamChaser.TheDrownedRuinsPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Modifier (ModifierType (DoesNotExhaustToCommit, ThreatModifier))
import LordOfTheRings.Phase
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

argaladCode, beravorCode, eowynCode, theodredCode, legolasCode :: CardCode
argaladCode = "12082"
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
legolasCode = "01005"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- enemyIds
  run . AddToStaging =<< genEncounterCard def
  fromJustNote "no enemy" . find (`notElem` existing) <$> enemyIds

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  eid <- stagedEnemy def
  run $ EngageEnemy pid eid
  pure eid

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  existing <- getsGame (Map.keys . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . find (`notElem` existing) . Map.keys . (.entities.locations))

progressOn :: LocationId -> GameT Int
progressOn lid = (.progress) <$> getLocation lid

attachmentWithCode :: CardDef -> GameT Target
attachmentWithCode def =
  getsGame (toTarget . fromJustNote "no attachment" . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

attachedTo :: Target -> GameT Placement
attachedTo t = getsGame (maybe Unplaced (.placement) . find ((== t) . toTarget) . toList . (.entities.attachments))

allyGone :: Target -> GameT Bool
allyGone t = getsGame (notElem t . map toTarget . toList . (.entities.allies))

spec :: Spec
spec = describe "The Drowned Ruins player cards" do
  describe "Argalad" do
    let deck = deckList [argaladCode, eowynCode, theodredCode] testDeck.cards
    it "lowers a staged enemy's threat by his attack, damaging it when that brings it to 0" . gameTestWith deck $ \pid -> do
      argalad <- hero argaladCode
      eid <- stagedEnemy Enemies.forestSpider
      run $ ActionWindow pid
      chooseAbilityOf pid argalad
      getEnemyThreat eid `shouldReturn` 0
      getCharacterDamage (toTarget eid) `shouldReturn` 1

    it "deals no damage to an enemy whose threat stays above 0" . gameTestWith deck $ \pid -> do
      argalad <- hero argaladCode
      eid <- stagedEnemy Enemies.kingSpider
      run $ CreateEffect $ Effect GameSource (toTarget eid) [ThreatModifier 1] UntilEndOfPhase
      run $ ActionWindow pid
      chooseAbilityOf pid argalad
      getEnemyThreat eid `shouldReturn` 1
      getCharacterDamage (toTarget eid) `shouldReturn` 0

  describe "Dwarven Sellsword" do
    it "is discarded at the end of the round unless the group spends 1 leadership" . gameTest $ \pid -> do
      sellsword <- putAllyIntoPlay pid Allies.dwarvenSellsword
      run $ CheckWindows [mkWhen RoundEnds]
      allyGone sellsword `shouldReturn` True

    it "stays when the group spends 1 leadership" . gameTest $ \pid -> do
      sellsword <- putAllyIntoPlay pid Allies.dwarvenSellsword
      give theodredCode 1
      run $ CheckWindows [mkWhen RoundEnds]
      chooseLabel "$label.groupSpend" pid
      chooseTarget pid =<< hero theodredCode
      allyGone sellsword `shouldReturn` False
      (`shouldBe` 0) =<< getHeroResources . toId =<< getHeroWithCode theodredCode

  describe "Dúnedain Remedy" do
    it "heals its hero as it is attached, and moves for a resource, healing the next" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource beravor Damage 2
      run $ PlaceTokens GameSource theodred Damage 2
      give beravorCode 1
      _ <- playWith pid theodredCode 0 Attachments.dunedainRemedy
      chooseTarget pid beravor
      remedy <- attachmentWithCode Attachments.dunedainRemedy
      chooseAbilityOf pid remedy
      getCharacterDamage beravor `shouldReturn` 1
      -- the planning window, asked again, offers to move it
      chooseAbilityOf pid remedy
      chooseTarget pid theodred
      attachedTo remedy `shouldReturn` AttachedTo theodred
      chooseAbilityOf pid remedy
      getCharacterDamage theodred `shouldReturn` 1

  describe "Battle-fury" do
    let deck = deckList [legolasCode, eowynCode, theodredCode] testDeck.cards
    it "has a hero attack before staging, then commits it for 1 spirit" . gameTestWith deck $ \pid -> do
      eid <- engagedEnemy pid Enemies.forestSpider
      give legolasCode 1
      give eowynCode 1
      card <- putCardIntoHand pid Events.battleFury
      run $ BeginPhase QuestPhase
      chooseTarget pid (CardIdTarget card.id)
      theodred <- hero theodredCode
      chooseTarget pid theodred
      getCharacterDamage (toTarget eid) `shouldReturn` 1
      chooseLabel "$label.groupSpend" pid
      chooseTarget pid =<< hero eowynCode
      heroCommitted . toAttrs <$> getHeroWithCode theodredCode `shouldReturn` True

  describe "Woodland Courier" do
    it "places 2 progress on a Forest location" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.oldForestRoad
      courier <- putAllyIntoPlay pid Allies.woodlandCourier
      chooseAbilityOf pid courier
      progressOn lid `shouldReturn` 2

    it "places 1 progress on any other location" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.kneesOfTheMountain
      courier <- putAllyIntoPlay pid Allies.woodlandCourier
      chooseAbilityOf pid courier
      progressOn lid `shouldReturn` 1

  describe "Hithlain" do
    it "places 1 progress on its location after a player plays a Silvan ally from hand" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.oldForestRoad
      hithlain <- putAttachmentOn pid Attachments.hithlain (toTarget lid)
      _ <- playWith pid eowynCode 1 Allies.silvanRefugee
      chooseAbilityOf pid hithlain
      progressOn lid `shouldReturn` 1

  describe "Robin Smallburrow" do
    it "raises each enemy's engagement cost by the quest points of the location traveled to" . gameTest $ \pid -> do
      robin <- putAllyIntoPlay pid Allies.robinSmallburrow
      eid <- stagedEnemy Enemies.forestSpider
      lid <- stagedLocation Locations.oldForestRoad
      give eowynCode 1
      run $ TravelTo lid
      chooseAbilityOf pid robin
      getEngagementCost eid `shouldReturn` 28

  describe "Interrogation" do
    it "looks at the top cards for the trapped enemy's printed threat, and may discard 1" . gameTest $ \pid -> do
      eid <- engagedEnemy pid Enemies.forestSpider
      _ <- putAttachmentOn pid Attachments.forestSnare (toTarget eid)
      [a, b, c] <- setEncounterDeck [Locations.oldForestRoad, Enemies.blackForestBats, Locations.theBrownLands]
      card <- putCardIntoHand pid Events.interrogation
      give beravorCode 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid (CardIdTarget b.id)
      map (.id) <$> getEncounterDeck `shouldReturn` [a.id, c.id]
      map (.id) <$> getEncounterDiscard `shouldReturn` [b.id]

  describe "Strider" do
    it "gives +2 willpower while you control 5 or fewer characters" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.strider beravor
      getWillpower beravor `shouldReturn` 4
      -- 3 heroes is more than 2: Beravor still exhausts to commit
      getModifiers beravor >>= (`shouldSatisfy` notElem DoesNotExhaustToCommit)
      replicateM_ 3 $ putAllyIntoPlay pid Allies.guardOfTheCitadel
      getWillpower beravor `shouldReturn` 2
