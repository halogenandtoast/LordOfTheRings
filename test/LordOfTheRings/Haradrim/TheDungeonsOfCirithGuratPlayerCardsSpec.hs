module LordOfTheRings.Haradrim.TheDungeonsOfCirithGuratPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (cdQuestPoints)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (victoryDisplayL)
import TestImport

folcoCode, frodoCode, eowynCode, theodredCode, glorfindelCode, beravorCode :: CardCode
folcoCode = "17109"
frodoCode = "02025"
eowynCode = "01007"
theodredCode = "01002"
glorfindelCode = "01011"
beravorCode = "01012"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = do
  g <- getGame
  putGame $ g & playersL . ix pid . threatL .~ n

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  existing <- enemyIds
  run . AddToStaging =<< genEncounterCard def
  eid <- fromJustNote "no enemy" . find (`notElem` existing) <$> enemyIds
  run $ EngageEnemy pid eid
  pure eid

defeatSideQuest :: PlayerId -> CardDef -> GameT ()
defeatSideQuest pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PutSideQuestIntoPlay pid card
  qid <- getsGame (fromJustNote "no side quest" . fmap toId . find ((== card.id) . questCardId . toAttrs) . toList . (.entities.quests))
  run $ CompleteQuest qid

isExhausted :: Target -> GameT Bool
isExhausted t = getsGame (maybe False ((.allyExhausted) . toAttrs) . find ((== t) . toTarget) . toList . (.entities.allies))

-- | Put cards on top of the player's deck.
onTopOfDeck :: PlayerId -> [CardDef] -> GameT [Card]
onTopOfDeck pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (cards <>)
  pure cards

spec :: Spec
spec = describe "The Dungeons of Cirith Gurat player cards" do
  describe "Folco Boffin" do
    let deck = deckList [folcoCode, frodoCode, eowynCode] testDeck.cards
    it "costs 1 less threat for each Hobbit hero you control" . gameTestWith deck $ \pid -> do
      run Setup
      -- Folco 7 - 2, Frodo 7, Éowyn 9
      threatOf pid `shouldReturn` 21

    it "is discarded to reduce your threat by 7, once per game" . gameTestWith deck $ \pid -> do
      setThreat pid 30
      folco <- hero folcoCode
      run $ ActionWindow pid
      chooseAbilityOf pid folco
      threatOf pid `shouldReturn` 23
      getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` notElem folcoCode)

  describe "Knight of Dale" do
    it "spends a leadership resource to ready, once a round" . gameTest $ \pid -> do
      knight <- putAllyIntoPlay pid Allies.knightOfDale
      give theodredCode 2
      run $ Exhaust knight
      run $ ActionWindow pid
      chooseAbilityOf pid knight
      isExhausted knight `shouldReturn` False
      passActions pid
      run $ Exhaust knight
      run $ ActionWindow pid
      offersAbilityOf pid knight `shouldReturn` False

    it "readies for free with Valour, apart from its other limit" . gameTest $ \pid -> do
      knight <- putAllyIntoPlay pid Allies.knightOfDale
      setThreat pid 40
      give theodredCode 1
      replicateM_ 2 do
        run $ Exhaust knight
        run $ ActionWindow pid
        chooseAbilityOf pid knight
        isExhausted knight `shouldReturn` False
        passActions pid

  describe "Fierce Defense" do
    let deck = deckList ["01005", beravorCode, eowynCode] testDeck.cards
    it "deals 3 damage to a non-unique engaged enemy in the combat phase" . gameTestWith deck $ \pid -> do
      troll <- engagedEnemy pid Enemies.hillTroll
      defense <- putCardIntoHand pid Events.fierceDefense
      give "01005" 3
      run $ BeginPhase CombatPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget defense.id)
      getDamage (toTarget troll) `shouldReturn` 3

    it "with Valour, discards a non-unique engaged enemy in any action window" . gameTestWith deck $ \pid -> do
      troll <- engagedEnemy pid Enemies.hillTroll
      defense <- putCardIntoHand pid Events.fierceDefense
      give "01005" 3
      setThreat pid 40
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget defense.id)
      enemyIds >>= (`shouldSatisfy` notElem troll)

  describe "Eregion Survivor" do
    it "gets +1 willpower, attack and defense while your hand is empty" . gameTest $ \pid -> do
      survivor <- putAllyIntoPlay pid Allies.eregionSurvivor
      g <- getGame
      putGame $ g & playersL . ix pid . handL .~ []
      run $ CheckWindows []
      getWillpower survivor `shouldReturn` 2
      getAttack survivor `shouldReturn` 2
      _ <- putCardIntoHand pid Events.fierceDefense
      run $ CheckWindows []
      getWillpower survivor `shouldReturn` 1

  describe "Heirs of Eärendil" do
    let deck = deckList [glorfindelCode, beravorCode, eowynCode] testDeck.cards
    it "raises your threat by a non-unique location's quest points to discard it" . gameTestWith deck $ \pid -> do
      card <- genEncounterCard Locations.oldForestRoad
      run $ AddToStaging card
      heirs <- putCardIntoHand pid Events.heirsOfEarendil
      give eowynCode 1
      threatBefore <- threatOf pid
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget heirs.id)
      threatOf pid `shouldReturn` threatBefore + fromMaybe 0 (cdQuestPoints Locations.oldForestRoad)
      getsGame (length . (.entities.locations)) `shouldReturn` 0

  describe "Leaflock" do
    it "enters play exhausted, and gets +1 willpower for each damaged Ent you control" . gameTest $ \pid -> do
      leaflock <- putAllyIntoPlay pid Allies.leaflock
      isExhausted leaflock `shouldReturn` True
      getWillpower leaflock `shouldReturn` 0
      ent <- putAllyIntoPlay pid Allies.boomingEnt
      run $ PlaceTokens GameSource ent Damage 1
      run $ PlaceTokens GameSource leaflock Damage 1
      getWillpower leaflock `shouldReturn` 2

  describe "Legacy Blade" do
    it "gives its hero +1 attack for each side quest in the victory display, to +3" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.legacyBlade eowyn
      getAttack eowyn `shouldReturn` 1
      defeatSideQuest pid Quests.keepWatch
      getAttack eowyn `shouldReturn` 2
      more <- replicateM 3 (PlayerCard <$> genPlayerCard Quests.rallyTheWest (Just pid))
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (victoryDisplayL %~ (more <>))
      run $ CheckWindows []
      getAttack eowyn `shouldReturn` 4

  describe "Open the Armory" do
    it "adds a Weapon or Armor from the top 10 cards to your hand" . gameTest $ \pid -> do
      [blade] <- onTopOfDeck pid [Attachments.bladeOfGondolin]
      armory <- putCardIntoHand pid Events.openTheArmory
      give eowynCode 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget armory.id)
      chooseTarget pid (CardIdTarget blade.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem blade.id)

    it "with Valour, puts one from the top 5 cards into play" . gameTest $ \pid -> do
      [plate] <- onTopOfDeck pid [Attachments.citadelPlate]
      armory <- putCardIntoHand pid Events.openTheArmory
      setThreat pid 40
      give eowynCode 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget armory.id)
      chooseLabel "$label.valour" pid
      chooseTarget pid (CardIdTarget plate.id)
      chooseTarget pid =<< hero eowynCode
      getsGame (map toCardCode . toList . (.entities.attachments)) `shouldReturn` [toCardCode Attachments.citadelPlate]
