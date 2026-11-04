module LordOfTheRings.VengeanceOfMordor.AShadowInTheEastPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (toCard)
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithEngagementCostAboveThreatOf))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (QuestPhase))
import TestImport

frodoCode, eowynCode, theodredCode, beravorCode :: CardCode
frodoCode = "21002"
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"

frodoDeck :: DeckList
frodoDeck = deckList [frodoCode, eowynCode, theodredCode] testDeck.cards

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . fmap enemyId . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no location" . fmap toId . find ((== card.id) . (.id) . toCard) . toList . (.entities.locations))

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))

placementOf :: EnemyId -> GameT (Maybe Placement)
placementOf eid = getsGame (fmap (enemyPlacement . toAttrs) . Map.lookup eid . (.entities.enemies))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . Map.lookup hid . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  _ -> pure False

-- | The tests start without setup, so the threat starts at 0.
threatTo :: PlayerId -> Int -> GameT ()
threatTo pid n = do
  t <- (.threat) <$> getPlayer pid
  run $ RaiseThreat pid (n - t)

spec :: Spec
spec = describe "A Shadow in the East player cards" do
  describe "Frodo Baggins (Leadership)" do
    it "spends a resource to ready another committed character, and lowers threat once the quest succeeds" . gameTestWith frodoDeck $ \pid -> do
      threatTo pid 10
      give frodoCode 1
      frodo <- hero frodoCode
      eowyn <- hero eowynCode
      run $ BeginPhase QuestPhase
      chooseTarget pid eowyn
      chooseTarget pid frodo
      chooseLabel "$label.doneCommitting" pid
      chooseAbilityOf pid frodo
      isExhausted eowyn `shouldReturn` False
      (getHeroResources . toId =<< getHeroWithCode frodoCode) `shouldReturn` 0
      (.threat) <$> getPlayer pid `shouldReturn` 10
      -- 6 willpower against no threat
      passActions pid
      (.threat) <$> getPlayer pid `shouldReturn` 9

    it "lowers no threat when the players quest unsuccessfully" . gameTestWith frodoDeck $ \pid -> do
      threatTo pid 10
      give frodoCode 1
      _ <- stagedEnemy Enemies.hillTroll
      _ <- stagedEnemy Enemies.hillTroll
      _ <- stagedEnemy Enemies.hillTroll
      _ <- stagedEnemy Enemies.hillTroll
      _ <- stagedEnemy Enemies.hillTroll
      _ <- stagedEnemy Enemies.hillTroll
      _ <- stagedEnemy Enemies.hillTroll
      frodo <- hero frodoCode
      eowyn <- hero eowynCode
      run $ BeginPhase QuestPhase
      chooseTarget pid eowyn
      chooseTarget pid frodo
      chooseLabel "$label.doneCommitting" pid
      chooseAbilityOf pid frodo
      -- 6 willpower against the trolls' 7 threat
      passActions pid
      (.threat) <$> getPlayer pid `shouldReturn` 11

    it "is not offered without another exhausted unique character committed" . gameTestWith frodoDeck $ \pid -> do
      give frodoCode 1
      frodo <- hero frodoCode
      run $ BeginPhase QuestPhase
      chooseTarget pid frodo
      chooseLabel "$label.doneCommitting" pid
      offersAbilityOf pid frodo `shouldReturn` False

  describe "Merry" do
    it "gives each unique ally you control +1 willpower for the round" . gameTest $ \pid -> do
      faramir <- putAllyIntoPlay pid Allies.faramir
      spearman <- putAllyIntoPlay pid Allies.gondorianSpearman
      merry <- putAllyIntoPlay pid Allies.merryLeadership
      chooseAbilityOf pid merry
      getWillpower faramir `shouldReturn` 3
      getWillpower merry `shouldReturn` 3
      getWillpower spearman `shouldReturn` 0

    it "can be the Rohan hero or Merry Horn of the Mark attaches to" . gameTest $ \pid -> do
      merry <- putAllyIntoPlay pid Allies.merryLeadership
      cardHosts pid (toCardCode Attachments.hornOfTheMark) >>= (`shouldSatisfy` elem merry)

  describe "Pippin" do
    it "readies after you engage an enemy with engagement cost above your threat" . gameTest $ \pid -> do
      pippin <- putAllyIntoPlay pid Allies.pippinTactics
      run $ Exhaust pippin
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      chooseAbilityOf pid pippin
      isExhausted pippin `shouldReturn` False

    it "gets +2 attack against an enemy with engagement cost above your threat" . gameTest $ \pid -> do
      pippin <- putAllyIntoPlay pid Allies.pippinTactics
      getModifiers pippin >>= (`shouldSatisfy` elem (AttackBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf pid) 2))

  describe "Faramir (Lore)" do
    it "engages a non-unique enemy in the staging area and deals it 2 damage" . gameTest $ \pid -> do
      eid <- stagedEnemy Enemies.hillTroll
      faramir <- putAllyIntoPlay pid Allies.faramirLore
      chooseAbilityOf pid faramir
      placementOf eid `shouldReturn` Just (EngagedWith pid)
      enemyDamage eid `shouldReturn` 2

    it "deals 4 damage instead to one with an attachment" . gameTest $ \pid -> do
      eid <- stagedEnemy Enemies.hillTroll
      _ <- putAttachmentOn pid Attachments.forestSnare (EnemyTarget eid)
      faramir <- putAllyIntoPlay pid Allies.faramirLore
      chooseAbilityOf pid faramir
      enemyDamage eid `shouldReturn` 4

    it "is not offered for a unique enemy" . gameTest $ \pid -> do
      _ <- stagedEnemy Enemies.louis
      faramir <- putAllyIntoPlay pid Allies.faramirLore
      offersAbilityOf pid faramir `shouldReturn` False

  describe "Derufin" do
    it "discards himself to lower the engaged player's threat by the destroyed enemy's printed threat" . gameTest $ \pid -> do
      threatTo pid 10
      derufin <- putAllyIntoPlay pid Allies.derufin
      eid <- stagedEnemy Enemies.dolGuldurOrcs
      run $ EngageEnemy pid eid
      run $ PlayerAttacks pid
      chooseTarget pid derufin
      chooseTarget pid =<< hero beravorCode
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      chooseAbilityOf pid derufin
      -- Dol Guldur Orcs' printed threat is 2
      (.threat) <$> getPlayer pid `shouldReturn` 8
      map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` [toCardCode Allies.derufin]

  describe "The Gathering Storm" do
    it "adds an enemy from the top 5 to the staging area, lowering the next ally's cost by its threat" . gameTest $ \pid -> do
      [_, spider] <- setEncounterDeck [Locations.oldForestRoad, Enemies.forestSpider]
      give theodredCode 2
      faramir <- putCardIntoHand pid Allies.faramir
      _ <- playWith pid theodredCode 0 Events.theGatheringStorm
      chooseTarget pid spider
      getsGame (map (toCardCode . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.forestSpider]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.oldForestRoad]
      -- Faramir's 4 cost less the spider's 2 threat
      chooseTarget pid (CardIdTarget faramir.id)
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 0
      getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.faramir]

  describe "Put Off Pursuit" do
    it "gives the active location +2 quest points" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.oldForestRoad
      run $ PlaceLocation lid ActiveLocation
      _ <- putAttachmentOn pid Attachments.putOffPursuit (LocationTarget lid)
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem (QuestPointModifier 2))

    it "attaches only to the active location" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.oldForestRoad
      cardHosts pid (toCardCode Attachments.putOffPursuit) `shouldReturn` []
      run $ PlaceLocation lid ActiveLocation
      cardHosts pid (toCardCode Attachments.putOffPursuit) `shouldReturn` [LocationTarget lid]

    it "as its location is explored, stages the next location to shuffle a non-unique enemy into the deck" . gameTest $ \pid -> do
      bats <- stagedEnemy Enemies.blackForestBats
      lid <- stagedLocation Locations.oldForestRoad
      run $ PlaceLocation lid ActiveLocation
      pursuit <- putAttachmentOn pid Attachments.putOffPursuit (LocationTarget lid)
      _ <- setEncounterDeck [Enemies.kingSpider, Locations.mountainsOfMirkwood]
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
      chooseAbilityOf pid pursuit
      -- the explored Old Forest Road joins the discarded King Spider
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.oldForestRoad, toCardCode Enemies.kingSpider]
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.mountainsOfMirkwood]
      getsGame (Map.member bats . (.entities.enemies)) `shouldReturn` False
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.blackForestBats]
