module LordOfTheRings.EredMithrin.CampaignSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (AttackModifier, DefenseModifier, IgnoreDeep, IgnoreDiscover, WillpowerModifier))
import LordOfTheRings.Outcome (Outcome (..))
import LordOfTheRings.Card (CardDef (..), CardType (..), cdEncounterSetQuantity, toCard, toCardDef)
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveTokens))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (fireInTheNight, journeyUpTheAnduin, lostInMirkwood, mountGundabad, roamAcrossRhovanion, theFateOfWilderland, theGhostOfFramsburg, theKingsQuest, theWitheredHeath)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), victoryDisplayL)
import LordOfTheRings.Trait (Trait (Hazard))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

emLog :: CampaignLog
emLog = (newCampaignLog "ered-mithrin") {campaignLogHeroes = [["01012", "01007", "01002"]]}

wonFirst :: [ScenarioId] -> CampaignLog -> CampaignLog
wonFirst sids l = l {campaignLogCompleted = [CompletedScenario s 100 | s <- sids]}

-- | The log with every treasure discovered but Jewels of Wilderland.
allButJewels :: CampaignLog
allButJewels = record Treasures (filter (/= toCardCode Attachments.jewelsOfWilderland) (map toCardCode Attachments.eredMithrinTreasures)) emLog

poolCodes :: CampaignLog -> [CardCode]
poolCodes = map (.code) . (.pool)

pureIt :: String -> IO () -> Spec
pureIt = it

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

deckCodes :: GameT [CardCode]
deckCodes = map toCardCode <$> getEncounterDeck

-- | How many of the cards pass the test.
countIn :: [EncounterCard] -> (CardDef -> Bool) -> Int
countIn cards f = length $ filter (f . toCardDef) cards

nonUnique :: CardType -> CardDef -> Bool
nonUnique t d = cdCardType d == t && not (cdUnique d)

isType :: CardType -> CardDef -> Bool
isType t d = cdCardType d == t

-- | Answer each "remove from the game" question with its first card.
removeFirstCards :: PlayerId -> GameT ()
removeFirstCards pid =
  getQuestion pid >>= \case
    Just (QuestionLabel l _ _ _) | "$label.eredMithrin.removeFromGame" `isPrefixOf` l -> do
      chooseFirstCard "remove a card" pid
      removeFirstCards pid
    _ -> pure ()

chooseFirstCard :: Text -> PlayerId -> GameT ()
chooseFirstCard reason pid = chooseOptionMatching reason pid \case
  TargetLabel (CardIdTarget _) _ -> True
  _ -> False

alliesInPlay :: GameT [CardCode]
alliesInPlay = getsGame (map toCardCode . toList . (.entities.allies))

attachedTo :: Target -> GameT [CardCode]
attachedTo host = getsGame (map toCardCode . filter ((== AttachedTo host) . (.placement)) . toList . (.entities.attachments))

-- | Urdug, objective-ally side, under the player.
putUrdugInPlay :: PlayerId -> GameT AllyId
putUrdugInPlay pid = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.urdug
  getsGame (fromJustNote "no Urdug" . headMay . map toId . filter ((== toCardCode Objectives.urdug) . toCardCode) . toList . (.entities.allies))

urdugResources :: AllyId -> GameT Int
urdugResources aid = getsGame (maybe 0 (countTokens Resource . allyTokens . toAttrs) . Map.lookup aid . (.entities.allies))

spec :: Spec
spec = describe "Ered Mithrin campaign" do
  describe "the campaign log" do
    pureIt "plays the cycle's nine scenarios in order" do
      nextScenario emLog `shouldBe` Just "journey-up-the-anduin"
      nextScenario (wonFirst ["journey-up-the-anduin", "lost-in-mirkwood"] emLog) `shouldBe` Just "the-kings-quest"
      campaignCardFor emLog "mount-gundabad" `shouldBe` Just "EMC193"

    pureIt "bars the Haldan hero, save in The King's Quest" do
      seatHeroes [["01012", "01007", "19026"]] emLog `shouldBe` Left ["19026"]
      let kingsQuest = wonFirst ["journey-up-the-anduin", "lost-in-mirkwood"] emLog
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "19026"]] kingsQuest `shouldBe` Right 0

    pureIt "lets heroes change freely at The King's Quest and The Ghost of Framsburg" do
      let ghost = wonFirst ["journey-up-the-anduin", "lost-in-mirkwood", "the-kings-quest", "the-withered-heath", "roam-across-rhovanion", "fire-in-the-night"] emLog
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "01001"]] ghost `shouldBe` Right 0
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "01001"]] (wonFirst ["journey-up-the-anduin"] emLog) `shouldBe` Right 1

    pureIt "writes each treasure discovered once" do
      let l = updateLog (MarkTreasure "EMC196") $ updateLog (MarkTreasure "EMC196") emLog
      recorded Treasures l `shouldBe` ["EMC196"]

    pureIt "adds no more copies of an encounter card to the pool than a collection holds" do
      -- Hill Troll: 1 copy in Wilderlands
      let add = updateLog (AddEncounterCardToPool $ toCardCode Enemies.hillTroll)
          owned = fromMaybe 1 (cdEncounterSetQuantity Enemies.hillTroll)
      length (poolCodes (foldr (const add) emLog [0 .. owned])) `shouldBe` owned

    pureIt "takes one copy out of the pool at a time" do
      let card = PoolCard (toCardCode Locations.twilightHall) Nothing Nothing
          l = emLog {campaignLogPool = [card, card]}
      length (updateLog (RemoveOneFromPool card) l).pool `shouldBe` 1

  describe "Journey Up the Anduin" do
    it "offers Turayn and, declined, notes his oath; A Growing Threat and the Evil Creatures in play join the pool" . campaignTestIn journeyUpTheAnduin emLog testDeck $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.hillTroll
      run $ GameOver Won
      chooseLabel "$label.eredMithrin.allyDeclined" pid
      l <- getCampaignLog
      Map.member TuraynsOath l.notes `shouldBe` True
      poolCodes l `shouldBe` ["EMC167", toCardCode Enemies.hillTroll]

    it "puts a resource on the card for a non-unique location explored, then a treasure, discovered once won" . campaignTestIn journeyUpTheAnduin allButJewels {campaignLogCardResources = 3} testDeck $ \pid -> do
      run $ UseCardAbility pid ScenarioSource 1 []
      (.cardResources) <$> getCampaignLog `shouldReturn` 4
      chooseOptionMatching "choose the treasure" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      beravor <- getHeroWithCode "01012"
      chooseTarget pid beravor.id
      map toCardCode . toList <$> getsGame (.entities.attachments) `shouldReturn` [toCardCode Attachments.jewelsOfWilderland]
      run $ GameOver Won
      chooseLabel "$label.eredMithrin.allyDeclined" pid
      l <- getCampaignLog
      toCardCode Attachments.jewelsOfWilderland `elem` recorded Treasures l `shouldBe` True
      l.cardResources `shouldBe` 0

  describe "treasure before the game" do
    it "lets a discovered treasure go into a player's deck, whoever discovered it" . campaignTestIn fireInTheNight (record Treasures [toCardCode Attachments.jewelsOfWilderland] emLog) testDeck $ \pid -> do
      run $ DoStep 2 Setup
      chooseTarget pid pid
      deck <- (.deck) <$> getPlayer pid
      toCardCode Attachments.jewelsOfWilderland `elem` map toCardCode deck `shouldBe` True

  describe "Fire in the Night" do
    it "ignores the deep keyword and lets each player take a non-burden card out of the pool" . campaignTestIn fireInTheNight emLog {campaignLogPool = [PoolCard (toCardCode Locations.twilightHall) Nothing Nothing]} testDeck $ \pid -> do
      run $ CheckWindows []
      getModifiers GameTarget >>= (`shouldSatisfy` elem IgnoreDeep)
      run $ GameOver Won
      chooseLabel "$label.eredMithrin.allyDeclined" pid
      chooseOptionMatching "remove Twilight Hall" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      l <- getCampaignLog
      poolCodes l `shouldBe` []
      Map.member BeldisCare l.notes `shouldBe` True

  describe "Mount Gundabad" do
    it "ignores the discover keyword; takes A Growing Threat out of the pool and, with 6 treasures, adds Dragon-sickness" . campaignTestIn mountGundabad allButJewels {campaignLogPool = [PoolCard "EMC167" Nothing Nothing]} testDeck $ \pid -> do
      run $ CheckWindows []
      getModifiers GameTarget >>= (`shouldSatisfy` elem IgnoreDiscover)
      run $ GameOver Won
      chooseLabel "$label.pass" pid
      poolCodes <$> getCampaignLog `shouldReturn` ["EMC194"]

  describe "Lost in Mirkwood" do
    it "gives Melanwar to a player, and may add Twilight Hall to the staging area guarding a random undiscovered treasure" . campaignTestIn lostInMirkwood allButJewels testDeck $ \pid -> do
      run $ DoStep 1 Setup
      halls <- length . filter (== toCardCode Locations.twilightHall) <$> deckCodes
      run $ DoStep 9 Setup
      chooseLabel "$label.eredMithrin.searchTwilightHall" pid
      -- the one undiscovered treasure, Jewels of Wilderland
      chooseFirstCard "choose the treasure" pid
      alliesInPlay >>= (`shouldSatisfy` elem "EMC169")
      hall <- getsGame (fromJustNote "no Twilight Hall" . find ((== toCardCode Locations.twilightHall) . toCardCode) . toList . (.entities.locations))
      hall.placement `shouldBe` StagingArea
      guarded <- Map.lookup (toCard hall).id . scenarioGuardedPlayerCards <$> scenarioAttrs
      map (toCardCode . snd) (fromMaybe [] guarded) `shouldBe` [toCardCode Attachments.jewelsOfWilderland]
      (length . filter (== toCardCode Locations.twilightHall) <$> deckCodes) `shouldReturn` halls - 1

  describe "The King's Quest" do
    it "gives Isolde to a player, adds The Threat to Wilderland with 3 resources, shuffles in Dagnir's Spawn and Abandoned Ruin, and removes 3 treacheries for Turayn's oath" . campaignTestIn theKingsQuest (record TuraynsOath ["EMC166"] (wonFirst ["journey-up-the-anduin", "lost-in-mirkwood"] emLog)) testDeck $ \pid -> do
      run $ DoStep 1 Setup
      treacheries <- (`countIn` isType TreacheryType) <$> getEncounterDeck
      run $ DoStep 9 Setup
      removeFirstCards pid
      alliesInPlay >>= (`shouldSatisfy` elem "EMC171")
      threatObj <- getsGame (fromJustNote "no Threat to Wilderland" . find ((== "EMC172") . toCardCode) . toList . (.entities.objectives))
      threatObj.placement `shouldBe` StagingArea
      countTokens Resource (objectiveTokens $ toAttrs threatObj) `shouldBe` 3
      deckCodes >>= (`shouldSatisfy` elem "EMC173")
      map toCardCode . (.cavesDeck) <$> scenarioAttrs >>= (`shouldSatisfy` elem "EMC174")
      ((`countIn` isType TreacheryType) <$> getEncounterDeck) `shouldReturn` treacheries - 3

  describe "The Withered Heath" do
    it "removes locations for Melanwar's guidance and enemies for Isolde's hunt, and flips its card" . campaignTestIn theWitheredHeath (record IsoldeOnTheHunt ["EMC171"] $ record MelanwarsGuidance ["EMC169"] emLog) testDeck $ \pid -> do
      run $ DoStep 1 Setup
      deck0 <- getEncounterDeck
      caves0 <- (.cavesDeck) <$> scenarioAttrs
      run $ DoStep 9 Setup
      chooseLabel "$label.pass" pid
      removeFirstCards pid
      deck1 <- getEncounterDeck
      caves1 <- (.cavesDeck) <$> scenarioAttrs
      countIn deck1 (nonUnique LocationType) `shouldBe` countIn deck0 (nonUnique LocationType) - 2
      countIn deck1 (nonUnique EnemyType) `shouldBe` countIn deck0 (nonUnique EnemyType) - 3
      length caves1 `shouldBe` max 0 (length caves0 - 1)
      scenarioCampaignCardFlipped <$> scenarioAttrs `shouldReturn` True

    it "offers a player without an Accolade the stage's Accolade as a main quest stage is defeated" . campaignTestIn theWitheredHeath emLog testDeck $ \pid -> do
      qid <- toId <$> (putQuestStage Quests.searchingForDragonSign >> getActiveQuest)
      beravor <- getHeroWithCode "01012"
      run $ CompleteQuest qid
      chooseTarget pid beravor.id
      attachedTo (toTarget beravor.id) `shouldReturn` [toCardCode Attachments.masterOfTheHunt]

    it "offers no Accolade to a player who controls one" . campaignTestIn theWitheredHeath emLog testDeck $ \pid -> do
      beravor <- getHeroWithCode "01012"
      _ <- putAttachmentOn pid Attachments.unbroken (toTarget beravor.id)
      qid <- toId <$> (putQuestStage Quests.searchingForDragonSign >> getActiveQuest)
      run $ CompleteQuest qid
      -- the quest deck empty, the game is won: the resolution asks for a Song, no Accolade
      -- is offered first
      getQuestion pid >>= (`shouldSatisfy` \case
        Just (QuestionLabel l _ _ _) -> not ("$label.eredMithrin.accolade" `isPrefixOf` l)
        _ -> True)
      attachedTo (toTarget beravor.id) `shouldReturn` [toCardCode Attachments.unbroken]

    -- Stage 4 "cannot be defeated while Cold-Drake is in play. When Cold-Drake is defeated,
    -- the players win the game": stage 4 is never defeated, so Dragon-slayer is not offered
    it "adds the Accolades in play and a Song to the pool, and never offers Dragon-slayer" . campaignTestIn theWitheredHeath emLog testDeck $ \pid -> do
      beravor <- getHeroWithCode "01012"
      _ <- putAttachmentOn pid Attachments.masterOfTheHunt (toTarget beravor.id)
      run $ GameOver Won
      getQuestion pid >>= (`shouldSatisfy` \case
        Just (QuestionLabel l _ _ _) -> "$label.eredMithrin.chooseSong" `isPrefixOf` l
        _ -> False)
      chooseFirstCard "choose a Song" pid
      l <- getCampaignLog
      [(p.code, p.attachedTo) | p <- l.pool, p.code == toCardCode Attachments.masterOfTheHunt] `shouldBe` [(toCardCode Attachments.masterOfTheHunt, Just "01012")]
      poolCodes l `shouldSatisfy` any (`elem` ["EMC180", "EMC181", "EMC182", "EMC183"])
      poolCodes l `shouldSatisfy` notElem (toCardCode Attachments.dragonSlayer)

    it "with 4 Dragon Signs in the victory display, offers a random undiscovered treasure as it is resolved" . campaignTestIn theWitheredHeath allButJewels testDeck $ \pid -> do
      signs <- replicateM 4 (EncounterCard <$> genEncounterCard Objectives.dragonSign)
      putGame . (scenarioL %~ overAttrs (victoryDisplayL .~ signs)) =<< getGame
      run $ GameOver Won
      chooseLabel "$label.pass" pid
      chooseFirstCard "choose the treasure" pid
      beravor <- getHeroWithCode "01012"
      chooseTarget pid beravor.id
      l <- getCampaignLog
      recorded Treasures l `shouldSatisfy` elem (toCardCode Attachments.jewelsOfWilderland)

  describe "Roam Across Rhovanion" do
    it "removes enemies for Melanwar's guidance, none a Grey Mountain Giant, and lets the first player trust Urdug" . campaignTestIn roamAcrossRhovanion (record MelanwarsGuidance ["EMC169"] emLog) testDeck $ \pid -> do
      run $ DoStep 1 Setup
      urdug <- putUrdugInPlay pid
      deck0 <- getEncounterDeck
      let giants cards = length [c | c <- cards, cdName (toCardDef c) == "Grey Mountain Giant"]
      run $ DoStep 9 Setup
      chooseLabel "$label.pass" pid
      removeFirstCards pid
      chooseLabel "$label.eredMithrin.trustUrdug" pid
      deck1 <- getEncounterDeck
      countIn deck1 (nonUnique EnemyType) `shouldBe` countIn deck0 (nonUnique EnemyType) - 3
      giants deck1 `shouldBe` giants deck0
      urdugResources urdug `shouldReturn` 1
      getModifiers (toTarget urdug) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1])
      scenarioCampaignCardFlipped <$> scenarioAttrs `shouldReturn` True

    it "shuffles Urdug's Cunning in if the first player does not trust Urdug" . campaignTestIn roamAcrossRhovanion emLog testDeck $ \pid -> do
      run $ DoStep 1 Setup
      _ <- putUrdugInPlay pid
      run $ DoStep 9 Setup
      chooseLabel "$label.pass" pid
      chooseLabel "$label.eredMithrin.distrustUrdug" pid
      deckCodes >>= (`shouldSatisfy` elem (toCardCode Treacheries.urdugsCunning))

    it "lets the first player trust Urdug again after claiming an objective" . campaignTestIn roamAcrossRhovanion emLog testDeck $ \pid -> do
      urdug <- putUrdugInPlay pid
      beravor <- getHeroWithCode "01012"
      -- free of encounters, the first player claims Durin's Key, attaching it to a hero
      run . AddToStaging =<< genEncounterCard Objectives.durinsKey
      chooseTarget pid beravor.id
      chooseLabel "$label.eredMithrin.trustUrdug" pid
      urdugResources urdug `shouldReturn` 1

    it "adds Misplaced Trust for each player with 3 resources on Urdug, and a chosen staged location" . campaignTestIn roamAcrossRhovanion emLog testDeck $ \pid -> do
      urdug <- putUrdugInPlay pid
      run $ PlaceTokens GameSource (toTarget urdug) Resource 3
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      run $ GameOver Won
      chooseOptionMatching "choose a staged location" pid \case
        TargetLabel (LocationTarget _) _ -> True
        _ -> False
      l <- getCampaignLog
      [(p.code, p.seat) | p <- l.pool] `shouldBe` [(toCardCode Treacheries.misplacedTrust, Just 0), (toCardCode Locations.oldForestRoad, Nothing)]

    it "adds Urdug's Cunning for each player with fewer than 3 resources on Urdug" . campaignTestIn roamAcrossRhovanion emLog testDeck $ \pid -> do
      urdug <- putUrdugInPlay pid
      run $ PlaceTokens GameSource (toTarget urdug) Resource 2
      run $ GameOver Won
      getQuestion pid >>= (`shouldSatisfy` isNothing)
      poolCodes <$> getCampaignLog `shouldReturn` [toCardCode Treacheries.urdugsCunning]

  describe "The Ghost of Framsburg" do
    it "removes treacheries for Isolde's hunt and locations for Beldis's care, and shuffles in its three burdens" . campaignTestIn theGhostOfFramsburg (record BeldisCare ["EMC188"] $ record IsoldeOnTheHunt ["EMC171"] emLog) testDeck $ \pid -> do
      run $ DoStep 1 Setup
      deck0 <- getEncounterDeck
      run $ DoStep 9 Setup
      chooseLabel "$label.pass" pid
      removeFirstCards pid
      deck1 <- getEncounterDeck
      countIn deck1 (isType TreacheryType) `shouldBe` countIn deck0 (isType TreacheryType) - 3
      countIn deck1 (nonUnique LocationType) `shouldBe` countIn deck0 (nonUnique LocationType) - 3
      map toCardCode deck1 `shouldSatisfy` \codes -> all (`elem` codes) ["EMC190", "EMC191", "EMC192"]
      scenarioCampaignCardFlipped <$> scenarioAttrs `shouldReturn` True

    it "attaches a random undiscovered treasure to Fram's Shade as it engages at stage 1B, once per game" . campaignTestIn theGhostOfFramsburg allButJewels testDeck $ \pid -> do
      qid <- toId <$> (putQuestStage Quests.theHauntedKeep >> getActiveQuest)
      putGame . (entitiesL . questsL . ix qid %~ overAttrs (\q -> q {questFlipped = True})) =<< getGame
      run . AddToStaging =<< genEncounterCard Enemies.framsShade
      shade <- getsGame (fromJustNote "no Fram's Shade" . headMay . toList . (.entities.enemies))
      run $ EngageEnemy pid (toId shade)
      chooseLabel "$label.eredMithrin.guardTreasure" pid
      chooseFirstCard "choose the treasure" pid
      guarded <- Map.lookup (toCard shade).id . scenarioGuardedPlayerCards <$> scenarioAttrs
      map (toCardCode . snd) (fromMaybe [] guarded) `shouldBe` [toCardCode Attachments.jewelsOfWilderland]
      run $ PlaceEnemy (toId shade) StagingArea
      run $ EngageEnemy pid (toId shade)
      getQuestion pid >>= (`shouldSatisfy` isNothing)

    it "adds Gundabad Hunter or Stalker, and 1 of 2 Hazard cards, to the pool" . campaignTestIn theGhostOfFramsburg emLog testDeck $ \pid -> do
      run $ DoStep 1 Setup
      run $ GameOver Won
      chooseOptionMatching "choose Gundabad Hunter" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      getQuestion pid >>= \case
        Just _ -> chooseFirstCard "choose a Hazard card" pid
        Nothing -> pure ()
      l <- getCampaignLog
      take 1 (poolCodes l) `shouldBe` [toCardCode Enemies.gundabadHunter]
      map (\c -> Hazard `member` cdCardTraits (toCardDef c)) (drop 1 (poolCodes l)) `shouldBe` [True]

  describe "The Fate of Wilderland" do
    it "ignores the deep and discover keywords, and ends the campaign once won" . campaignTestIn theFateOfWilderland (wonFirst ["journey-up-the-anduin", "lost-in-mirkwood", "the-kings-quest", "the-withered-heath", "roam-across-rhovanion", "fire-in-the-night", "the-ghost-of-framsburg", "mount-gundabad"] emLog) testDeck $ \_ -> do
      run $ CheckWindows []
      mods <- getModifiers GameTarget
      (IgnoreDeep `elem` mods, IgnoreDiscover `elem` mods) `shouldBe` (True, True)
      run $ GameOver Won
      nextScenario <$> getCampaignLog `shouldReturn` Nothing
