module LordOfTheRings.DreamChaser.CampaignSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally (Ally)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (objectiveTokens)
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (raidOnTheGreyHavens, templeOfTheDeceived, theCityOfCorsairs, theFateOfNumenor, theThingInTheDepths, voyageAcrossBelegaer)
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), scenarioCampaignCardFlipped, scenarioMode, victoryDisplayL)
import LordOfTheRings.Window (WindowType (..), mkAfter)
import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

dc :: CampaignLog
dc = newCampaignLog "dream-chaser"

seated :: CampaignLog
seated = dc {campaignLogHeroes = [["01001", "01002", "01003"]]}

withVictory :: [CardDef] -> GameT ()
withVictory defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (map EncounterCard cards <>))) =<< getGame

encounterDeckCodes :: GameT [CardCode]
encounterDeckCodes = getsGame (map toCardCode . (.encounterDeck) . toAttrs . (.scenario))

calphons :: GameT [Ally]
calphons = getsGame (filter ((== toCardCode Objectives.calphon) . toCardCode) . toList . (.entities.allies))

attachedTo :: CardDef -> GameT [Target]
attachedTo def =
  getsGame \g -> [t | a <- toList g.entities.attachments, toCardCode a == toCardCode def, AttachedTo t <- [attachmentPlacement (toAttrs a)]]

-- | A test of pure values, in IO.
pureIt :: String -> IO () -> Spec
pureIt = it

spec :: Spec
spec = describe "Dream-chaser campaign" do
  describe "the campaign log" do
    pureIt "plays The Grey Havens' scenarios, then the cycle's" do
      nextScenario dc `shouldBe` Just "voyage-across-belegaer"
      nextScenario dc {campaignLogCompleted = [CompletedScenario s 0 | s <- ["voyage-across-belegaer", "the-fate-of-numenor", "raid-on-the-grey-havens"]]}
        `shouldBe` Just "flight-of-the-stormcaller"
      partNumber dc "the-city-of-corsairs" `shouldBe` 9

    pureIt "spends 1 XP to mark a checkbox, and upgrades a card once each is marked" do
      let l = dc {campaignLogXp = 2, campaignLogPool = [PoolCard "DCC189" Nothing (Just "01001")]}
          spent = spendXp "DCC172" =<< spendXp "DCC172" l
      (.xp) <$> spent `shouldBe` Just 0
      isUpgraded "DCC172" <$> spent `shouldBe` Just True
      upgradedCode <$> spent <*> pure "DCC172" `shouldBe` Just "DCC172U"
      -- a Permanent boon in the pool turns to its Upgraded side as its last box is marked
      let cutlass = foldM (flip spendXp) dc {campaignLogXp = 5, campaignLogPool = [PoolCard "DCC189" Nothing (Just "01001")]} (replicate 5 "DCC189" :: [CardCode])
      map (.code) . (.pool) <$> cutlass `shouldBe` Just ["DCC189U"]

    pureIt "spends no XP it does not have, nor past a card's checkboxes" do
      spendXp "DCC172" dc `shouldBe` Nothing
      (spendXp "DCC172" <=< spendXp "DCC172" <=< spendXp "DCC172") dc {campaignLogXp = 5} `shouldBe` Nothing

    pureIt "offers Gavin once A Storm on Cobas Haven is won" do
      map xpCard (xpCards dc) `shouldSatisfy` notElem "DCC210"
      map xpCard (xpCards dc {campaignLogCompleted = [CompletedScenario "a-storm-on-cobas-haven" 0]}) `shouldSatisfy` elem "DCC210"

    pureIt "reads a log kept before XP as having none" do
      let old = object ["campaign" .= ("dream-chaser" :: Text), "heroes" .= ([] :: [[Text]]), "fallenHeroes" .= ([] :: [Text]), "threatPenalty" .= (0 :: Int), "pool" .= ([] :: [Value]), "notes" .= object [], "completed" .= ([] :: [Value])]
      campaignLogXp <$> parseMaybe parseJSON old `shouldBe` Just 0

  describe "Voyage Across Belegaer" do
    it "shuffles Ominous Fate and Thunderstruck into the encounter deck" . campaignTestIn voyageAcrossBelegaer seated starterDeck $ \_ -> do
      run $ DoStep 9 Setup
      deck <- encounterDeckCodes
      deck `shouldSatisfy` elem (toCardCode Treacheries.ominousFate)
      deck `shouldSatisfy` elem (toCardCode Enemies.thunderstruck)

    it "adds Ominous Fate to the Campaign Pool and gains XP" . campaignTestIn voyageAcrossBelegaer seated starterDeck $ \_ -> do
      withVictory [Enemies.crowley, Enemies.thunderstruck]
      run $ GameOver Won
      l <- getCampaignLog
      map (.code) l.pool `shouldBe` [toCardCode Treacheries.ominousFate]
      -- 1 surviving player, no Ship enemy in play, Crowley 1 and Thunderstruck 2
      l.xp `shouldBe` 5
      getsGame (scenarioCampaignCardFlipped . toAttrs . (.scenario)) `shouldReturn` True

    it "loses 1 XP for Ominous Fate in the victory display" . campaignTestIn voyageAcrossBelegaer seated starterDeck $ \_ -> do
      withVictory [Treacheries.ominousFate]
      run $ GameOver Won
      (.xp) <$> getCampaignLog `shouldReturn` 1

  describe "The Fate of Númenor" do
    it "shuffles Revenant in, and gains XP for each resource on the campaign card" . campaignTestIn theFateOfNumenor seated starterDeck $ \pid -> do
      run $ DoStep 9 Setup
      -- no character is damaged for threat
      chooseLabel "$label.done" pid
      encounterDeckCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.revenant))
      revenant <- genEncounterCard Enemies.revenant
      run $ CheckWindows [mkAfter $ EnemyDestroyed (EncounterCard revenant)]
      (.cardResources) <$> getCampaignLog `shouldReturn` 1
      run $ GameOver Won
      l <- getCampaignLog
      -- 1 surviving player, 1 resource, threat 40 or less
      l.xp `shouldBe` 3
      l.cardResources `shouldBe` 0

  describe "Raid on the Grey Havens" do
    it "gives Calphon to the first player with Spyglass and Black Key, and a Galdor's Counsel to choose" . campaignTestIn raidOnTheGreyHavens seated starterDeck $ \pid -> do
      run $ DoStep 9 Setup
      calphon <- fromJustNote "no Calphon" . headMay <$> calphons
      calphon.placement `shouldBe` InPlayArea pid
      attachedTo Attachments.spyglass `shouldReturn` [toTarget calphon.id]
      attachedTo Attachments.blackKey `shouldReturn` [toTarget calphon.id]
      chooseOptionMatching "choose a Galdor's Counsel" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      hand <- map toCardCode . (.hand) <$> getPlayer pid
      let galdors = map toCardCode [Events.galdorsCounselLeadership, Events.galdorsCounselTactics, Events.galdorsCounselSpirit, Events.galdorsCounselLore]
      filter (`elem` galdors) hand `shouldSatisfy` ((== 1) . length)
      l <- getCampaignLog
      map (.seat) l.pool `shouldBe` [Just 0]
      length (recorded ChosenDuringSetup l) `shouldBe` 1
      encounterDeckCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.graxar))

    it "removes Calphon from the game once stage 2B is revealed, the game not lost" . campaignTestIn raidOnTheGreyHavens seated starterDeck $ \pid -> do
      run $ DoStep 9 Setup
      chooseLabel "$label.dreamChaserCampaign.decline" pid
      _ <- putQuestStage Quests.sahirsAdvance
      run $ CheckWindows [mkAfter AdvancedToStage]
      length <$> calphons `shouldReturn` 0
      attachedTo Attachments.spyglass `shouldReturn` []
      getsGame (.outcome) `shouldReturn` Nothing

    it "records Graxar defeated, for Parrying Cutlass or Throwing Axe after" . campaignTestIn raidOnTheGreyHavens seated starterDeck $ \_ -> do
      withVictory [Enemies.graxar]
      run $ GameOver Won
      l <- getCampaignLog
      recorded GraxarDefeated l `shouldBe` [toCardCode Enemies.graxar]
      Map.member ChosenDuringSetup l.notes `shouldBe` False

    it "attaches the chosen reward to a hero and records it in the pool" . campaignTestIn theFateOfNumenor seated {campaignLogNotes = Map.fromList [(GraxarDefeated, [toCardCode Enemies.graxar])]} starterDeck $ \pid -> do
      run $ DoStep 9 Setup
      chooseLabel "$label.dreamChaserCampaign.throwingAxe" pid
      theodred <- getHeroWithCode "01002"
      chooseTarget pid theodred.id
      attachedTo Attachments.throwingAxe `shouldReturn` [toTarget theodred.id]
      l <- getCampaignLog
      l.pool `shouldBe` [PoolCard (toCardCode Attachments.throwingAxe) Nothing (Just "01002")]
      recorded GraxarDefeated l `shouldBe` []

  describe "The Thing in the Depths" do
    it "keys Captain Sahír as he enters play, and as he turns over gives the boon, Sea Serpent and Come Together" . campaignTestIn theThingInTheDepths seated starterDeck $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.captainSahir
      sahir <- getsGame (fromJustNote "no Sahír" . fmap toId . find ((== toCardCode Enemies.captainSahir) . toCardCode) . toList . (.entities.enemies))
      run $ CheckWindows []
      blackKeys <- getsGame (filter ((== toCardCode Objectives.blackKeyBurden) . toCardCode) . toList . (.entities.objectives))
      map (.placement) blackKeys `shouldBe` [AttachedTo (toTarget sahir)]
      run $ Flip (toTarget sahir)
      sahirAlly <- getsGame (fromJustNote "no Sahír ally" . fmap toId . find ((== toCardCode Objectives.captainSahirObjective) . toCardCode) . toList . (.entities.allies))
      attachedTo Attachments.blackKey `shouldReturn` [toTarget sahirAlly]
      getsGame (map toCardCode . toList . (.entities.enemies)) >>= (`shouldSatisfy` elem (toCardCode Enemies.seaSerpent))
      hand <- map toCardCode . (.hand) <$> getPlayer pid
      hand `shouldSatisfy` elem (toCardCode Events.comeTogether)
      recorded ChosenDuringSetup <$> getCampaignLog `shouldReturn` [toCardCode Events.comeTogether]

  describe "The City of Corsairs" do
    it "attaches Black Key (burden) and The Black Sword, with 3 resources, as Captain Sahír enters play" . campaignTestIn theCityOfCorsairs seated starterDeck $ \_ -> do
      run . AddToStaging =<< genEncounterCard Enemies.captainSahirTheCityOfCorsairs
      sahir <- getsGame (fromJustNote "no Sahír" . fmap toId . find ((== toCardCode Enemies.captainSahirTheCityOfCorsairs) . toCardCode) . toList . (.entities.enemies))
      run $ CheckWindows []
      attached <- getsGame (filter ((== AttachedTo (toTarget sahir)) . (.placement)) . toList . (.entities.objectives))
      sort (map toCardCode attached) `shouldBe` sort [toCardCode Objectives.blackKeyBurden, toCardCode Objectives.theBlackSword]
      swords <- getsGame (filter ((== toCardCode Objectives.theBlackSword) . toCardCode) . toList . (.entities.objectives))
      map (countTokens Resource . objectiveTokens . toAttrs) swords `shouldBe` [3]

  describe "Recurring Nightmare" do
    let nightmare = toCardCode Enemies.recurringNightmare
        deckCodes pid = map toCardCode . (.deck) <$> getPlayer pid
        nightmares = getsGame (filter ((== nightmare) . toCardCode) . toList . (.entities.enemies))
    pureIt "stays out of the seat's deck list, the scenario shuffling it in" do
      seatDeck 0 dc {campaignLogPool = [PoolCard nightmare (Just 0) Nothing]} [("01013", 3)] `shouldBe` [("01013", 3)]

    it "is shuffled into a player's deck as it leaves play, and added to the staging area as it is drawn" . campaignTestIn templeOfTheDeceived seated starterDeck $ \pid -> do
      card <- genEncounterCard Enemies.recurringNightmare
      run $ AddToStaging card
      run . Destroy GameSource . toTarget . fromJustNote "no nightmare" . headMay =<< nightmares
      length <$> nightmares `shouldReturn` 0
      deckCodes pid >>= (`shouldSatisfy` elem nightmare)
      getsGame ((.encounterDiscard) . toAttrs . (.scenario)) `shouldReturn` []
      run $ MoveToTopOfDeck pid card.id
      run $ DrawCards (Drawing pid 1)
      map (.placement) <$> nightmares `shouldReturn` [StagingArea]
      map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` notElem nightmare)
      deckCodes pid >>= (`shouldSatisfy` notElem nightmare)

    it "goes to the encounter discard pile discarded from a player's deck" . campaignTestIn templeOfTheDeceived seated starterDeck $ \pid -> do
      card <- genEncounterCard Enemies.recurringNightmare
      run $ ShuffleCardIntoDeck pid (EncounterCard card)
      run $ MoveToTopOfDeck pid card.id
      run $ DiscardTopOfDeck pid 1
      getsGame (map toCardCode . (.encounterDiscard) . toAttrs . (.scenario)) `shouldReturn` [nightmare]
      map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` []

    it "is noted by Temple of the Deceived's resolution for each one not a secret" . campaignTestIn templeOfTheDeceived seated {campaignLogNotes = Map.fromList [(RevealedSecrets, [nightmare, toCardCode Allies.starla])]} starterDeck $ \_ -> do
      run $ GameOver Won
      l <- getCampaignLog
      recorded RecurringNightmares l `shouldBe` [nightmare]
      map (.code) l.pool `shouldSatisfy` notElem nightmare

    it "joins the pool for the chosen player as the next scenario is set up, into their deck" . campaignTestIn theThingInTheDepths seated {campaignLogNotes = Map.fromList [(RecurringNightmares, [nightmare])]} starterDeck $ \pid -> do
      run $ DoStep 9 Setup
      l <- getCampaignLog
      l.pool `shouldBe` [PoolCard nightmare (Just 0) Nothing]
      Map.member RecurringNightmares l.notes `shouldBe` False
      deckCodes pid >>= (`shouldSatisfy` elem nightmare)

    it "is shuffled into its player's deck after the starting hand, not the encounter deck" . campaignTestIn theThingInTheDepths seated {campaignLogPool = [PoolCard nightmare (Just 0) Nothing]} starterDeck $ \pid -> do
      run $ DoStep 2 Setup
      encounterDeckCodes >>= (`shouldSatisfy` notElem nightmare)
      deckCodes pid >>= (`shouldSatisfy` notElem nightmare)
      run $ DoStep 6 Setup
      deckCodes pid >>= (`shouldSatisfy` elem nightmare)

  describe "in nightmare mode" do
    it "keys Captain Sahír in The Thing in the Depths past the nightmare deck's own Forced effects" . campaignTestIn (overAttrs (\a -> a {scenarioMode = NightmareMode}) theThingInTheDepths) seated starterDeck $ \_ -> do
      run . AddToStaging =<< genEncounterCard Enemies.captainSahir
      sahir <- getsGame (fromJustNote "no Sahír" . fmap toId . find ((== toCardCode Enemies.captainSahir) . toCardCode) . toList . (.entities.enemies))
      run $ CheckWindows []
      blackKeys <- getsGame (filter ((== toCardCode Objectives.blackKeyBurden) . toCardCode) . toList . (.entities.objectives))
      map (.placement) blackKeys `shouldBe` [AttachedTo (toTarget sahir)]

    it "counts Revenant's resource in Temple of the Deceived past the nightmare deck's own Forced effects" . campaignTestIn (overAttrs (\a -> a {scenarioMode = NightmareMode}) templeOfTheDeceived) seated starterDeck $ \_ -> do
      revenant <- genEncounterCard Enemies.revenant
      run $ CheckWindows [mkAfter $ EnemyDestroyed (EncounterCard revenant)]
      (.cardResources) <$> getCampaignLog `shouldReturn` 1

