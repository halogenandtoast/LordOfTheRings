module LordOfTheRings.AngmarAwakened.WastesOfEriadorPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Window (WindowType (DealtDamage), mkAfter)
import TestImport

merryCode, beravorCode, theodredCode, eowynCode, aragornCode :: CardCode
merryCode = "10001"
beravorCode = "01012"
theodredCode = "01002"
eowynCode = "01007"
aragornCode = "01001"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

-- | Merry (spirit), Beravor (lore) and Théodred (leadership).
merryTest :: (PlayerId -> GameT ()) -> IO ()
merryTest = gameTestWith (deckList [merryCode, beravorCode, theodredCode] testDeck.cards)

inHand :: PlayerId -> CardId -> GameT Bool
inHand pid cid = elem cid . map (.id) . (.hand) <$> getPlayer pid

allyWithCard :: CardId -> GameT (Maybe Target)
allyWithCard cid = getsGame (fmap toTarget . find ((== cid) . allyCardId . toAttrs) . toList . (.entities.allies))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  _ -> pure False

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  run . AddToStaging =<< genEncounterCard def
  eid <- getsGame (fromJustNote "no enemy" . headMay . Map.keys . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

spec :: Spec
spec = describe "The Wastes of Eriador player cards" do
  describe "Merry (spirit)" do
    it "exhausts to reduce your threat by the threat of an enemy revealed" . merryTest $ \pid -> do
      setThreat pid 30
      _ <- setEncounterDeck [Enemies.forestSpider]
      run RevealEncounterCard
      merry <- hero merryCode
      chooseAbilityOf pid merry
      threat pid `shouldReturn` 28
      isExhausted merry `shouldReturn` True

  describe "Ingold" do
    it "gets +1 willpower for each of your heroes with a resource" . gameTest $ \pid -> do
      ingold <- putAllyIntoPlay pid Allies.ingold
      getWillpower ingold `shouldReturn` 0
      hero beravorCode >>= \t -> run (PlaceTokens GameSource t Resource 1)
      hero theodredCode >>= \t -> run (PlaceTokens GameSource t Resource 2)
      getWillpower ingold `shouldReturn` 2

  describe "Rallying Cry" do
    it "returns an ally that left play to its owner's hand" . gameTest $ \pid -> do
      hero theodredCode >>= \t -> run (PlaceTokens GameSource t Resource 2)
      card <- putCardIntoHand pid Events.rallyingCry
      ally <- putAllyIntoPlay pid Allies.gondorianSpearman
      Just allyCard <- getsGame (fmap (allyCardId . toAttrs) . headMay . toList . (.entities.allies))
      run $ Discard GameSource ally
      chooseTarget pid (CardIdTarget card.id)
      inHand pid allyCard `shouldReturn` True

    it "as a Valour Action, sends each ally leaving play this phase to its owner's hand" . gameTest $ \pid -> do
      setThreat pid 40
      hero theodredCode >>= \t -> run (PlaceTokens GameSource t Resource 2)
      card <- putCardIntoHand pid Events.rallyingCry
      ally <- putAllyIntoPlay pid Allies.gondorianSpearman
      Just allyCard <- getsGame (fmap (allyCardId . toAttrs) . headMay . toList . (.entities.allies))
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      passActions pid
      run $ Discard GameSource ally
      inHand pid allyCard `shouldReturn` True

    it "is no action below 40 threat" . gameTest $ \pid -> do
      hero theodredCode >>= \t -> run (PlaceTokens GameSource t Resource 2)
      card <- putCardIntoHand pid Events.rallyingCry
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ ActionWindow pid
      getQuestion pid >>= (`shouldSatisfy` maybe True (notElem (CardIdTarget card.id) . targetsOf))

  describe "Honour Guard" do
    it "exhausts to cancel 1 damage just dealt to a character" . gameTest $ \pid -> do
      guard' <- putAllyIntoPlay pid Allies.honourGuard
      beravor <- hero beravorCode
      run $ PlaceTokens GameSource beravor Damage 3
      run $ CheckWindows [mkAfter $ DealtDamage beravor 3]
      chooseAbilityOf pid guard'
      getCharacterDamage beravor `shouldReturn` 2

    it "with Valour, discards to cancel up to 5 damage" . gameTest $ \pid -> do
      setThreat pid 40
      guard' <- putAllyIntoPlay pid Allies.honourGuard
      beravor <- hero beravorCode
      run $ PlaceTokens GameSource beravor Damage 3
      run $ CheckWindows [mkAfter $ DealtDamage beravor 3]
      chooseOptionMatching "use the Valour response" pid \case
        AbilityLabel _ ab _ _ -> ab.abilityIndex == 2
        _ -> False
      getCharacterDamage beravor `shouldReturn` 0
      allyWithCard' guard' `shouldReturn` False

  describe "Raven-winged Helm" do
    it "exhausts to prevent 1 damage just dealt to its hero" . gameTestWith (deckList [aragornCode, beravorCode, theodredCode] testDeck.cards) $ \pid -> do
      aragorn <- hero aragornCode
      helm <- putAttachmentOn pid Attachments.ravenWingedHelm aragorn
      run $ PlaceTokens GameSource aragorn Damage 2
      run $ CheckWindows [mkAfter $ DealtDamage aragorn 2]
      chooseAbilityOf pid helm
      getCharacterDamage aragorn `shouldReturn` 1

  describe "Curious Brandybuck" do
    it "is put into play from hand after you travel, and goes to the bottom of the deck as the active location is explored" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Allies.curiousBrandybuck
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame (fromJustNote "no location" . headMay . Map.keys . (.entities.locations))
      run $ TravelTo lid
      chooseTarget pid (CardIdTarget card.id)
      Just _ <- allyWithCard card.id
      inHand pid card.id `shouldReturn` False
      -- the Old Forest Road's own response to the travel, declined
      chooseOptionMatching "decline" pid \case
        Label {} -> True
        _ -> False
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      allyWithCard card.id `shouldReturn` Nothing
      (fmap (.id) . lastMay . (.deck) <$> getPlayer pid) `shouldReturn` Just card.id

  describe "Hobbit Pony" do
    it "commits its exhausted hero to the quest as a Quest Action" . merryTest $ \pid -> do
      merry <- hero merryCode
      pony <- putAttachmentOn pid Attachments.hobbitPony merry
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ ActionWindow pid
      chooseAbilityOf pid pony
      heroCommitted . toAttrs <$> getHeroWithCode merryCode `shouldReturn` True
      isExhausted merry `shouldReturn` True

  describe "East Road Ranger" do
    it "gets +2 willpower while committed to a side quest" . gameTest $ \pid -> do
      ranger <- putAllyIntoPlay pid Allies.eastRoadRanger
      _ <- putQuestStage Quests.fliesAndSpiders
      run $ CommitToQuest ranger
      getWillpower ranger `shouldReturn` 1
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      _ <- setEncounterDeck [Quests.searchTheRuins, Locations.oldForestRoad]
      run RevealEncounterCard
      side <- getsGame (toTarget . fromJustNote "no side quest" . find ((== toCardCode Quests.searchTheRuins) . toCardCode) . toList . (.entities.quests))
      run $ SetCurrentQuest side
      getWillpower ranger `shouldReturn` 3

  describe "Scout Ahead" do
    it "adds a card worth no victory points from the top of the encounter deck to the victory display" . gameTest $ \pid -> do
      [ufthak, spider, road] <- setEncounterDeck [Enemies.chieftanUfthak, Enemies.forestSpider, Locations.oldForestRoad]
      card <- genPlayerCard Quests.scoutAhead (Just pid)
      run $ PutSideQuestIntoPlay pid card
      qid <- getsGame (fromJustNote "no side quest" . headMay . Map.keys . (.entities.quests))
      run $ CompleteQuest qid
      Just q <- getQuestion pid
      sort (targetsOf q) `shouldBe` sort [CardIdTarget spider.id, CardIdTarget road.id]
      chooseTarget pid (CardIdTarget spider.id)
      chooseTarget pid (CardIdTarget road.id)
      chooseTarget pid (CardIdTarget ufthak.id)
      victory <- getsGame (map (.id) . scenarioVictoryDisplay . toAttrs . (.scenario))
      victory `shouldSatisfy` elem spider.id
      map (.id) <$> getEncounterDeck `shouldReturn` [ufthak.id, road.id]

  describe "Ranger of Cardolan" do
    it "is put into play from hand for 1 resource after you engage an enemy, and shuffled away at the end of the round" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Allies.rangerOfCardolan
      hero theodredCode >>= \t -> run (PlaceTokens GameSource t Resource 1)
      _ <- engagedEnemy pid Enemies.forestSpider
      chooseTarget pid (CardIdTarget card.id)
      Just _ <- allyWithCard card.id
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 0
      run EndRound
      allyWithCard card.id `shouldReturn` Nothing
      elem card.id . map (.id) . (.deck) <$> getPlayer pid `shouldReturn` True

    it "is not offered without a Dúnedain hero" . gameTestWith (deckList [eowynCode, theodredCode] testDeck.cards) $ \pid -> do
      card <- putCardIntoHand pid Allies.rangerOfCardolan
      hero theodredCode >>= \t -> run (PlaceTokens GameSource t Resource 1)
      _ <- engagedEnemy pid Enemies.forestSpider
      getQuestion pid >>= (`shouldSatisfy` maybe True (notElem (CardIdTarget card.id) . targetsOf))
 where
  targetsOf q = [t | TargetLabel t _ <- questionChoices q]
  allyWithCard' t = getsGame (any ((== t) . toTarget) . toList . (.entities.allies))
