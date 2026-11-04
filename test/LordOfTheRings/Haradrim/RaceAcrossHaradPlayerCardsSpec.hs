module LordOfTheRings.Haradrim.RaceAcrossHaradPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.I18n (ikeyWith, intVar)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getMainQuestProgress)
import LordOfTheRings.Modifier (ModifierType (DoesNotContributeThreat, GainTrait))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (victoryDisplayL)
import LordOfTheRings.Trait (Trait (Warrior))
import TestImport

thurindirCode, beravorCode, eowynCode, theodredCode, gimliCode :: CardCode
thurindirCode = "17028"
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
gimliCode = "01004"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

resources :: CardCode -> GameT Int
resources code = getHeroResources . toId =<< getHeroWithCode code

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
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

intoVictoryDisplay :: Card -> GameT ()
intoVictoryDisplay card = do
  putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (card :))) =<< getGame
  run $ CheckWindows []

handIds :: PlayerId -> GameT [CardId]
handIds pid = map (.id) . (.hand) <$> getPlayer pid

spec :: Spec
spec = describe "Race Across Harad player cards" do
  describe "Thurindir" do
    let deck = deckList [thurindirCode, beravorCode, theodredCode] (testDeck.cards <> [("16013", 1)])
    it "searches his deck for a side quest at setup" . gameTestWith deck $ \pid -> do
      sideQuest <- fromJustNote "no side quest" . find ((== "16013") . toCardCode) . (.deck) <$> getPlayer pid
      run Setup
      chooseTarget pid (CardIdTarget sideQuest.id)
      chooseLabel "$label.keepHand" pid
      elem sideQuest.id <$> handIds pid `shouldReturn` True

    it "gets +1 willpower for each side quest in the victory display" . gameTestWith deck $ \pid -> do
      thurindir <- hero thurindirCode
      intoVictoryDisplay . EncounterCard =<< genEncounterCard Quests.orcRearguard
      intoVictoryDisplay . PlayerCard =<< genPlayerCard Quests.exploreSecretWays (Just pid)
      getWillpower thurindir `shouldReturn` 4

  describe "Kahliel's Tribesman" do
    it "exhausts to give another Harad character +1 willpower, attack and defense" . gameTest $ \pid -> do
      tribesman <- putAllyIntoPlay pid Allies.kahlielsTribesman
      yazan <- putAllyIntoPlay pid Allies.yazan
      run $ ActionWindow pid
      chooseAbilityOf pid tribesman
      getWillpower yazan `shouldReturn` 3
      getWillpower tribesman `shouldReturn` 1

  describe "Steed of the North" do
    it "readies its hero after you engage an enemy" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      steed <- putAttachmentOn pid Attachments.steedOfTheNorth beravor
      run $ Exhaust beravor
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      chooseAbilityOf pid steed
      isExhausted beravor `shouldReturn` False

  describe "Mighty Warrior" do
    let deck = deckList [gimliCode, beravorCode, theodredCode] testDeck.cards
    it "gives the Warrior trait, and draws a card as it is played from hand" . gameTestWith deck $ \pid -> do
      beravor <- hero beravorCode
      card <- playWith pid gimliCode 1 Attachments.mightyWarrior
      chooseTarget pid beravor
      was <- length <$> handIds pid
      warrior <- getsGame (toTarget . fromJustNote "no attachment" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.attachments))
      chooseAbilityOf pid warrior
      length <$> handIds pid `shouldReturn` was + 1
      getModifiers beravor >>= (`shouldSatisfy` elem (GainTrait Warrior))

  describe "Proud Hunters" do
    -- a 0-cost card still needs a resource match: Gimli (core) is the tactics hero
    it "adds the destroyed enemy's printed threat to the pool of a hero that attacked it" . gameTestWith (deckList [gimliCode, beravorCode, eowynCode] testDeck.cards) $ \pid -> do
      beravor <- hero beravorCode
      eid <- stagedEnemy Enemies.forestSpider
      run $ PlaceEnemy eid (EngagedWith pid)
      run $ PlaceTokens GameSource (toTarget eid) Damage 3
      card <- putCardIntoHand pid Events.proudHunters
      run $ PlayerAttacks pid
      chooseTarget pid beravor
      chooseLabel "$label.doneAttacking" pid
      -- the action window before the attack's damage
      chooseLabel "$label.pass" pid
      chooseTarget pid (CardIdTarget card.id)
      resources beravorCode `shouldReturn` 2

  describe "Dúnedain Pathfinder" do
    it "adds a non-unique location from the top 5 cards of the encounter deck to the staging area" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad]
      pathfinder <- putAllyIntoPlay pid Allies.dunedainPathfinder
      getsGame (length . (.entities.locations)) `shouldReturn` 1
      getsGame (elem pathfinder . map toTarget . toList . (.entities.allies)) `shouldReturn` True

    it "is discarded when no location enters play" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider]
      _ <- putAllyIntoPlay pid Allies.dunedainPathfinder
      getsGame (null . (.entities.allies)) `shouldReturn` True

  describe "Backtrack" do
    it "moves X progress from the main quest to a location in the staging area" . gameTest $ \pid -> do
      main <- putQuestStage Quests.fliesAndSpiders
      run $ PlaceTokens GameSource main Progress 3
      lid <- stagedLocation Locations.oldForestRoad
      _ <- playWith pid eowynCode 1 Events.backtrack
      chooseLabel (ikeyWith "label.xEquals" [intVar "x" 2]) pid
      (.progress) <$> getLocation lid `shouldReturn` 2
      getMainQuestProgress `shouldReturn` 1

  describe "Eryn Galen Settler" do
    it "discards a staged location with the explored active location's title" . gameTest $ \pid -> do
      settler <- putAllyIntoPlay pid Allies.erynGalenSettler
      active <- stagedLocation Locations.oldForestRoad
      run $ TravelTo active
      -- Old Forest Road's own response to the travel
      chooseLabel "$label.pass" pid
      other <- stagedLocation Locations.oldForestRoad
      run $ PlaceTokens GameSource (toTarget active) Progress 3
      chooseAbilityOf pid settler
      getsGame (Map.member other . (.entities.locations)) `shouldReturn` False
      getsGame (null . (.entities.allies)) `shouldReturn` True

  describe "Explore Secret Ways" do
    it "keeps staged locations sharing the active location's title out of the staging threat" . gameTest $ \pid -> do
      active <- stagedLocation Locations.oldForestRoad
      run $ TravelTo active
      -- Old Forest Road's own response to the travel
      chooseLabel "$label.pass" pid
      same <- stagedLocation Locations.oldForestRoad
      different <- stagedLocation Locations.theBrownLands
      intoVictoryDisplay . PlayerCard =<< genPlayerCard Quests.exploreSecretWays (Just pid)
      getModifiers same >>= (`shouldSatisfy` elem DoesNotContributeThreat)
      getModifiers different >>= (`shouldSatisfy` notElem DoesNotContributeThreat)

  describe "Steward of Orthanc" do
    it "gives an event Doomed 1 to draw a card, once a round" . gameTest $ \pid -> do
      steward <- putAllyIntoPlay pid Allies.stewardOfOrthanc
      putGame . (playersL . ix pid . threatL .~ 30) =<< getGame
      again <- putCardIntoHand pid Events.loreOfImladris
      give beravorCode 2
      was <- length <$> handIds pid
      _ <- playWith pid beravorCode 2 Events.loreOfImladris
      chooseAbilityOf pid steward
      (.threat) <$> getPlayer pid `shouldReturn` 31
      -- the event played, a card drawn
      length <$> handIds pid `shouldReturn` was + 1
      chooseTarget pid (CardIdTarget again.id)
      offersAbilityOf pid steward `shouldReturn` False
