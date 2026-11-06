module LordOfTheRings.Dreadnaught.FleetSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Attachment.Types (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Card (CardType (AllyType), cdDeckLimit, cdName)
import LordOfTheRings.Contract (Contract, createContract)
import LordOfTheRings.Contract.CardDefs qualified as Contracts
import LordOfTheRings.Decklist (deckProblem, withContract)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Entities (contractsL)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Window (getActionAbilities)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Matcher (CardMatcher (CardWithType), getAlly)
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Scenarios.TheHuntForTheDreadnaught (theHuntForTheDreadnaught)
import LordOfTheRings.Scenario.Types (GameMode (HardMode), modeL, encounterDiscardL)
import LordOfTheRings.Trait (Trait (Rohan))
import TestImport

ship :: PlayerId -> CardDef -> GameT AllyId
ship pid def = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "ship missing"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.allies)
    )

voyage :: PlayerId -> GameT Contract
voyage pid = do
  card <- genPlayerCard Contracts.aPerilousVoyage (Just pid)
  let c = createContract pid card
  g <- getGame
  putGame =<< preloadModifiers (g & entitiesL . contractsL %~ insertEntity c)
  pure c

voyageCards :: PlayerId -> [CardDef] -> GameT [Card]
voyageCards pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL .~ cards
  pure cards

spec :: Spec
spec = describe "Dreadnaught fleet" do
  it "Eithiliant boosts its controller's heroes and copies another hero's printed traits" . gameTest $ \pid -> do
    _ <- ship pid Objectives.eithiliant
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid beravor
    chooseTarget pid eowyn
    getWillpower beravor `shouldReturn` 3
    getModifiers beravor >>= (`shouldSatisfy` elem (GainTrait Rohan))

  it "Golden Wing reduces threat during setup, grants ranged, and improves heroes on upgrade"
    . gameTest
    $ \pid -> do
      run $ DoStep 2 Setup
      aid <- ship pid Objectives.goldenWing
      beravor <- toTarget <$> getHeroWithCode "01012"
      chooseTarget pid beravor
      (.threat) <$> getPlayer pid `shouldReturn` 24
      getHitPoints beravor `shouldReturn` 5
      getModifiers beravor >>= (`shouldSatisfy` elem (GainKeyword Ranged))
      run $ Flip (toTarget aid)
      getHitPoints beravor `shouldReturn` 6

  it "keeps damage, exhaustion and controller when a ship upgrades" . gameTest $ \pid -> do
    aid <- ship pid Objectives.silentMist
    run $ PlaceTokens GameSource (toTarget aid) Damage 3
    run $ Exhaust (toTarget aid)
    run $ Flip (toTarget aid)
    changed <- getAlly aid
    toCardCode changed `shouldBe` toCardCode Objectives.silentMistUpgraded
    getDamage (toTarget aid) `shouldReturn` 3
    (toAttrs changed).allyExhausted `shouldBe` True
    changed.placement `shouldBe` InPlayArea pid
    getHitPoints (toTarget aid) `shouldReturn` 17

  it "changes only the selected player's starting hand" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
    _ <- ship one Objectives.prideOfLebennin
    _ <- ship two Objectives.silentMist
    getStartingHandSizeFor one `shouldReturn` 11
    getStartingHandSizeFor two `shouldReturn` 8
    getModifiers (PlayerTarget one) >>= (`shouldSatisfy` elem CannotMulligan)
    getModifiers (PlayerTarget two) >>= (`shouldSatisfy` notElem CannotMulligan)

  it "Pride of Lebennin draws eleven, returns five, and offers no mulligan" . gameTest $ \pid -> do
    _ <- ship pid Objectives.prideOfLebennin
    run $ DoStep 5 Setup
    length . (.hand) <$> getPlayer pid `shouldReturn` 11
    replicateM_ 5 do
      hand <- (.hand) <$> getPlayer pid
      chooseTarget pid $ CardIdTarget $ (headEx hand).id
    length . (.hand) <$> getPlayer pid `shouldReturn` 6
    question <- getQuestion pid
    let mulligan = any (\case Label "$label.mulligan" _ -> True; _ -> False) . questionChoices
    fmap mulligan question `shouldBe` Just False

  it "Silent Mist plays an affordable five-cost event for four resources" . gameTest $ \pid -> do
    aid <- ship pid Objectives.silentMist
    card <- putCardIntoHand pid Events.lightTheBeacons
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 4
    run $ UseCardAbility pid (AllySource aid) 1 []
    chooseTarget pid $ CardIdTarget card.id
    player <- getPlayer pid
    map toCardCode player.discard `shouldSatisfy` elem (toCardCode Events.lightTheBeacons)
    hero <- getHeroWithCode "01007"
    getHeroResources (toId hero) `shouldReturn` 0
    getModifiers eowyn >>= (`shouldSatisfy` elem (DefenseModifier 2))

  it "Twilight's Call ignores resource matches only until its controller plays a card" . gameTest $ \pid -> do
    aid <- ship pid Objectives.twilightsCall
    _ <- putCardIntoHand pid Allies.gondorianSpearman
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 2
    playableCardsByEffect pid (CardWithType AllyType) 0
      >>= (`shouldSatisfy` any ((== toCardCode Allies.gondorianSpearman) . toCardCode))
    run
      $ CreateEffect
      $ Effect GameSource (PlayerTarget pid) [PlayedCardThisRound "01023" AllyType] UntilEndOfRound
    playableCardsByEffect pid (CardWithType AllyType) 0 `shouldReturn` []
    run $ Flip (toTarget aid)
    playableCardsByEffect pid (CardWithType AllyType) 0
      >>= (`shouldSatisfy` any ((== toCardCode Allies.gondorianSpearman) . toCardCode))

  it "collection additions honor the physical deck's copy limit" . gameTest $ \pid -> do
    run $ AddCollectionCardToHand pid (toCardCode Events.sneakAttack)
    length . (.hand) <$> getPlayer pid `shouldReturn` 1
    run $ AddCollectionCardToHand pid (toCardCode Events.sneakAttack)
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "Swan Ship grants setup resources and pays two resources to ready a character once per round" . gameTest $ \pid -> do
    aid <- ship pid Objectives.swanShipOfDolAmroth
    beravor <- getHeroWithCode "01012"
    eowyn <- getHeroWithCode "01007"
    theodred <- getHeroWithCode "01002"
    getHeroResources (toId beravor) `shouldReturn` 1
    getHeroResources (toId eowyn) `shouldReturn` 1
    getHeroResources (toId theodred) `shouldReturn` 1
    run $ RemoveTokens GameSource (toTarget eowyn) Resource 1
    run $ RemoveTokens GameSource (toTarget theodred) Resource 1
    run $ PlaceTokens GameSource (toTarget beravor) Resource 1
    run $ Exhaust (toTarget theodred)
    abilities <- getActionAbilities pid
    let ability = fromJustNote "Swan Ship action" $ find ((== AllySource aid) . abilitySource) abilities
    run $ UseAbility pid ability []
    chooseTarget pid $ toTarget theodred
    getHeroResources (toId beravor) `shouldReturn` 0
    refreshed <- getHeroWithCode "01002"
    (toAttrs refreshed).heroExhausted `shouldBe` False
    run $ Exhaust (toTarget theodred)
    run $ PlaceTokens GameSource (toTarget beravor) Resource 2
    getActionAbilities pid >>= (`shouldSatisfy` all ((/= AllySource aid) . abilitySource))

  it "Golden Wing's offered upgrade response damages an enemy" . gameTest $ \pid -> do
    aid <- ship pid Objectives.goldenWing
    chooseTarget pid =<< getHeroWithCode "01012"
    run . AddToStaging =<< genEncounterCard Enemies.fierceMarauder
    run $ Flip (toTarget aid)
    chooseAbilityOf pid aid
    chooseOptionMatching "local enemy" pid (const True)
    getsGame (length . (.entities.enemies)) `shouldReturn` 0

  it "Eithiliant still places six progress when its controller shuffles no discard cards" . gameTest $ \pid -> do
    aid <- ship pid Objectives.eithiliant
    chooseTarget pid =<< getHeroWithCode "01012"
    chooseTarget pid =<< getHeroWithCode "01007"
    run . AddToStaging =<< genEncounterCard Locations.belfalasShipyard
    run $ Flip (toTarget aid)
    chooseAbilityOf pid aid
    chooseLabel "$label.done" pid
    chooseOptionMatching "local location" pid (const True)
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.belfalasShipyard))

  it "Pride of Lebennin's upgrade heals and moves two encounter discards to victory" . gameTest $ \pid -> do
    aid <- ship pid Objectives.prideOfLebennin
    hero <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget hero) Damage 2
    cards <- traverse genEncounterCard [Enemies.forestSpider, Locations.oldForestRoad]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
    run $ Flip (toTarget aid)
    chooseAbilityOf pid aid
    for_ cards $ chooseTarget pid
    getDamage (toTarget hero) `shouldReturn` 0
    getEncounterDiscard `shouldReturn` []
    victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
    sort (map toCardCode victory) `shouldBe` sort (map toCardCode cards)

  it "Silent Mist's upgrade returns an event from discard and allows stopping after one" . gameTest $ \pid -> do
    aid <- ship pid Objectives.silentMist
    card <- putCardIntoHand pid Events.sneakAttack
    run $ DiscardFromHand pid card.id
    run $ Flip (toTarget aid)
    chooseAbilityOf pid aid
    chooseTarget pid card
    chooseLabel "$label.done" pid
    map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` notElem card.id)

  it "Swan Ship's upgrade adds resources and readies an attachment" . gameTest $ \pid -> do
    aid <- ship pid Objectives.swanShipOfDolAmroth
    hero <- getHeroWithCode "01002"
    attachment <- putAttachmentOn pid Attachments.stewardOfGondor (toTarget hero)
    run $ Exhaust attachment
    run $ Flip (toTarget aid)
    chooseAbilityOf pid aid
    chooseTarget pid hero
    chooseTarget pid attachment
    getHeroResources (toId hero) `shouldReturn` 3
    attachments <- getsGame (toList . (.entities.attachments))
    map (attachmentExhausted . toAttrs) attachments `shouldBe` [False]

  it "Twilight's Call draws three cards when its upgrade response is taken" . gameTest $ \pid -> do
    aid <- ship pid Objectives.twilightsCall
    handSize <- length . (.hand) <$> getPlayer pid
    run $ Flip (toTarget aid)
    chooseAbilityOf pid aid
    length . (.hand) <$> getPlayer pid `shouldReturn` handSize + 3

  it "A Perilous Voyage requires at least one hundred cards, excluding the contract" . gameTest $ \_ -> do
    let uniquePrintings =
          Map.elems
            $ Map.fromList [(title, def) | def <- Map.elems Allies.allAllyCards, let Name title _ = cdName def]
        physicalCards = concat [[toCardCode def | _ <- [1 .. cdDeckLimit def]] | def <- uniquePrintings]
        voyageDeck size =
          withContract (toCardCode Contracts.aPerilousVoyage)
            $ deckList ["01012", "01007", "01002"]
            $ Map.toList
            $ Map.fromListWith (+) [(code, 1) | code <- take size physicalCards]
    deckProblem (voyageDeck 99) `shouldSatisfy` isJust
    deckProblem (voyageDeck 100) `shouldBe` Nothing
    deckProblem (voyageDeck 101) `shouldBe` Nothing

  it "A Perilous Voyage bars player-card threat reduction on side A but permits encounter reduction" . gameTest $ \pid -> do
    _ <- voyage pid
    run $ RaiseThreat pid 20
    hero <- getHeroWithCode "01012"
    runQueueT $ Lifted.lowerThreatBy hero pid 5
    void mainLoop
    (.threat) <$> getPlayer pid `shouldReturn` 20
    runQueueT $ Lifted.lowerThreat pid 5
    void mainLoop
    (.threat) <$> getPlayer pid `shouldReturn` 15

  it "A Perilous Voyage draws the chosen top card and puts the other on the bottom" . gameTest $ \pid -> do
    c <- voyage pid
    [topCard, secondCard, thirdCard] <-
      voyageCards pid [Events.sneakAttack, Events.lightTheBeacons, Events.aTestOfWill]
    abilities <- getActionAbilities pid
    let action = fromJustNote "voyage action" $ find ((== ContractSource (toId c)) . abilitySource) abilities
    run $ UseAbility pid action []
    chooseTarget pid secondCard
    (.hand) <$> getPlayer pid `shouldReturn` [secondCard]
    (.deck) <$> getPlayer pid `shouldReturn` [thirdCard, topCard]
    (.threat) <$> getPlayer pid `shouldReturn` 1
    getActionAbilities pid >>= (`shouldSatisfy` all ((/= ContractSource (toId c)) . abilitySource))

  it "A Perilous Voyage flips instead of threat elimination and grants questing without exhaustion" . gameTest $ \pid -> do
    c <- voyage pid
    [card] <- voyageCards pid [Events.sneakAttack]
    run $ RaiseThreat pid 50
    (.threat) <$> getPlayer pid `shouldReturn` 40
    (.eliminated) <$> getPlayer pid `shouldReturn` False
    chooseTarget pid card
    chooseLabel "$label.aPerilousVoyage.take" pid
    (.hand) <$> getPlayer pid `shouldReturn` [card]
    hero <- getHeroWithCode "01012"
    getModifiers (toTarget hero) >>= (`shouldSatisfy` elem DoesNotExhaustToCommit)
    runQueueT $ Lifted.lowerThreatBy hero pid 5
    void mainLoop
    (.threat) <$> getPlayer pid `shouldReturn` 40
    runQueueT $ Lifted.lowerThreatBy c pid 5
    void mainLoop
    (.threat) <$> getPlayer pid `shouldReturn` 35

  it "A Perilous Voyage pays a searched card's reduced cost without moving it through hand" . gameTest $ \pid -> do
    c <- voyage pid
    [card] <- voyageCards pid [Allies.faramir]
    hero <- getHeroWithCode "01002"
    run $ PlaceTokens GameSource (toTarget hero) Resource 1
    run $ Flip (toTarget c)
    chooseTarget pid card
    chooseLabel "$label.aPerilousVoyage.play" pid
    getHeroResources (toId hero) `shouldReturn` 0
    (.hand) <$> getPlayer pid `shouldReturn` []
    (.deck) <$> getPlayer pid `shouldReturn` []
    getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.faramir]

  it "A Perilous Voyage does not offer an unaffordable searched card to play" . gameTest $ \pid -> do
    c <- voyage pid
    [card] <- voyageCards pid [Allies.faramir]
    run $ Flip (toTarget c)
    chooseTarget pid card
    Just question <- getQuestion pid
    questionChoices question
      `shouldSatisfy` all (\case Label "$label.aPerilousVoyage.play" _ -> False; _ -> True)
    chooseLabel "$label.aPerilousVoyage.take" pid
    (.hand) <$> getPlayer pid `shouldReturn` [card]

  it "A Perilous Voyage still needs a resource match when the discount makes a searched card free" . gameTest $ \pid -> do
    c <- voyage pid
    [card] <- voyageCards pid [Allies.gondorianSpearman]
    run $ Flip (toTarget c)
    chooseTarget pid card
    Just question <- getQuestion pid
    questionChoices question
      `shouldSatisfy` all (\case Label "$label.aPerilousVoyage.play" _ -> False; _ -> True)
    chooseLabel "$label.aPerilousVoyage.take" pid

  it "A Perilous Voyage's flip still obeys Hard Mode's threat-reduction limit" . gameTestIn (overAttrs (modeL .~ HardMode) theHuntForTheDreadnaught) testDeck $ \pid -> do
    _ <- voyage pid
    [card] <- voyageCards pid [Events.sneakAttack]
    run $ RaiseThreat pid 50
    (.threat) <$> getPlayer pid `shouldReturn` 49
    chooseTarget pid card
    chooseLabel "$label.aPerilousVoyage.take" pid
    (.eliminated) <$> getPlayer pid `shouldReturn` False

  it "A Perilous Voyage saves its controller only on the first threat-elimination flip" . gameTest $ \pid -> do
    _ <- voyage pid
    [card] <- voyageCards pid [Events.sneakAttack]
    run $ RaiseThreat pid 50
    chooseTarget pid card
    chooseLabel "$label.aPerilousVoyage.take" pid
    run $ RaiseThreat pid 10
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "A Perilous Voyage finishes its action when paying the threat cost flips it" . gameTest $ \pid -> do
    c <- voyage pid
    _ <- voyageCards pid [Events.sneakAttack, Events.aTestOfWill, Events.lightTheBeacons]
    run $ RaiseThreat pid 49
    abilities <- getActionAbilities pid
    let action = fromJustNote "voyage action" $ find ((== ContractSource (toId c)) . abilitySource) abilities
    run $ UseAbility pid action []
    chooseOptionMatching "search after flipping" pid (const True)
    chooseLabel "$label.aPerilousVoyage.take" pid
    chooseOptionMatching "draw from the remaining top two" pid (const True)
    (.threat) <$> getPlayer pid `shouldReturn` 40
    length . (.hand) <$> getPlayer pid `shouldReturn` 2
    length . (.deck) <$> getPlayer pid `shouldReturn` 1
