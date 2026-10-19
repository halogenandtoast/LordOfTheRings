module LordOfTheRings.Core.EventsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.I18n (ikeyWith, intVar)
import TestImport

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

revealed :: CardDef -> GameT ()
revealed def = setEncounterDeck [def] >> run RevealEncounterCard

spec :: Spec
spec = do
  describe "Blade Mastery" do
    it "gives a character +1 attack and +1 defense until the end of the phase" . gameTestWith (coreDeck "tactics") $ \pid -> do
      _ <- playWith pid "01004" 1 Events.bladeMastery
      gimli <- toTarget <$> getHeroWithCode "01004"
      chooseTarget pid gimli
      getAttack gimli `shouldReturn` 3
      getDefense gimli `shouldReturn` 3

  describe "Lore of Imladris" do
    it "heals all damage from a character" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ PlaceTokens GameSource eowyn Damage 2
      _ <- playWith pid "01012" 2 Events.loreOfImladris
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 0

  describe "Lórien's Wealth" do
    it "has a player draw 3 cards" . gameTest $ \pid -> do
      before <- handSize pid
      _ <- playWith pid "01012" 3 Events.loriensWealth
      handSize pid `shouldReturn` before + 3

  describe "Beorn's Hospitality" do
    it "heals all damage on each of a player's heroes" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      theodred <- toTarget <$> getHeroWithCode "01002"
      run $ PlaceTokens GameSource eowyn Damage 1
      run $ PlaceTokens GameSource theodred Damage 2
      _ <- playWith pid "01012" 5 Events.beornsHospitality
      getCharacterDamage eowyn `shouldReturn` 0
      getCharacterDamage theodred `shouldReturn` 0

  describe "$label.theGaladhrimsGreeting.title" do
    it "reduces one player's threat by 6" . gameTest $ \pid -> do
      run $ RaiseThreat pid 20
      _ <- playWith pid "01007" 3 Events.theGaladhrimsGreeting
      chooseLabel "$label.theGaladhrimsGreeting.one" pid
      (.threat) <$> getPlayer pid `shouldReturn` 14

  describe "A Light in the Dark" do
    it "returns an engaged enemy to the staging area" . gameTest $ \pid -> do
      revealed Enemies.forestSpider
      eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
      run $ EngageEnemy pid eid
      _ <- playWith pid "01007" 2 Events.aLightInTheDark
      chooseTarget pid eid
      (.placement) <$> getsGame (fromJustNote "gone" . headMay . toList . (.entities.enemies)) `shouldReturn` StagingArea

  describe "Radagast's Cunning" do
    it "is a quest action" . gameTest $ \pid -> do
      revealed Enemies.forestSpider
      card <- putCardIntoHand pid Events.radagastsCunning
      beravor <- getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget beravor) Resource 1
      run $ BeginPhase PlanningPhase
      Just question <- getQuestion pid
      [() | TargetLabel (CardIdTarget cid) _ <- questionChoices question, cid == card.id] `shouldBe` []

    it "stops an enemy contributing its threat until the end of the phase" . gameTest $ \pid -> do
      revealed Enemies.forestSpider
      card <- putCardIntoHand pid Events.radagastsCunning
      beravor <- getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget beravor) Resource 1
      g <- getGame
      putGame $ g & phaseL .~ QuestPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
      chooseTarget pid eid
      getStagingThreat `shouldReturn` 0

  describe "Strength of Will" do
    it "exhausts a Spirit character after travelling to place 2 progress" . gameTest $ \pid -> do
      revealed Locations.forestGate
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      _ <- putCardIntoHand pid Events.strengthOfWill
      run $ TravelTo lid
      chooseOptionMatching "play Strength of Will" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      (.progress) <$> getLocation lid `shouldReturn` 2

  describe "Stand and Fight" do
    it "puts an ally of cost X from a discard pile into play" . gameTestWith (coreDeck "spirit") $ \pid -> do
      tracker <- PlayerCard <$> genPlayerCard Allies.northernTracker (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . discardL .~ [tracker]
      _ <- playWith pid "01007" 4 Events.standAndFight
      chooseTarget pid (CardIdTarget tracker.id)
      getsGame (map (allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` [tracker.id]
      (.resources) <$> getHeroWithCode "01007" `shouldReturn` 0

  describe "Fortune or Fate" do
    it "puts a hero from a discard pile back into play" . gameTestWith (coreDeck "spirit") $ \pid -> do
      dunhere <- getHeroWithCode "01009"
      run $ Discard (toTarget dunhere)
      _ <- playWith pid "01007" 5 Events.fortuneOrFate
      chooseTarget pid (CardIdTarget $ heroCardId $ toAttrs dunhere)
      getsGame (length . (.entities.heroes)) `shouldReturn` 3

  describe "Dwarven Tomb" do
    it "returns a Spirit card from the discard pile to hand" . gameTestWith (coreDeck "spirit") $ \pid -> do
      testOfWill <- PlayerCard <$> genPlayerCard Events.aTestOfWill (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . discardL .~ [testOfWill]
      _ <- playWith pid "01007" 1 Events.dwarvenTomb
      chooseTarget pid (CardIdTarget testOfWill.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem testOfWill.id)

  describe "Will of the West" do
    it "shuffles a discard pile into the deck and leaves the game" . gameTestWith (coreDeck "spirit") $ \pid -> do
      testOfWill <- PlayerCard <$> genPlayerCard Events.aTestOfWill (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . discardL .~ [testOfWill]
      deck <- length . (.deck) <$> getPlayer pid
      _ <- playWith pid "01007" 1 Events.willOfTheWest
      (.discard) <$> getPlayer pid `shouldReturn` []
      length . (.deck) <$> getPlayer pid `shouldReturn` deck + 1

  describe "Gandalf's Search" do
    it "looks at the top X cards of a deck and adds one to hand" . gameTestWith (coreDeck "lore") $ \pid -> do
      deck <- (.deck) <$> getPlayer pid
      _ <- playWith pid "01012" 2 Events.gandalfsSearch
      chooseLabel (ikeyWith "label.xEquals" [intVar "x" 2]) pid
      let [top, second] = take 2 deck
      chooseTarget pid (CardIdTarget second.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem second.id)
      take 1 . map (.id) . (.deck) <$> getPlayer pid `shouldReturn` [top.id]
      (.resources) <$> getHeroWithCode "01012" `shouldReturn` 0
