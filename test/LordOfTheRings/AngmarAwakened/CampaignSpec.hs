module LordOfTheRings.AngmarAwakened.CampaignSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Modifier (ModifierType (RefreshThreatRaise))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (..))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), scenarioMode)
import LordOfTheRings.Scenario (acrossTheEttenmoors, deadmensDike, intrudersInChetwood, theBattleOfCarnDum, theDreadRealm, theTreacheryOfRhudaur, theWastesOfEriador, theWeatherHills)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

angmar :: CampaignLog
angmar = (newCampaignLog "angmar-awakened") {campaignLogHeroes = [["01012", "01007", "01002"]]}

-- | The log once the first scenarios are won, in order.
wonFirst :: [ScenarioId] -> CampaignLog -> CampaignLog
wonFirst sids l = l {campaignLogCompleted = [CompletedScenario sid 100 | sid <- sids]}

lostRealm :: [ScenarioId]
lostRealm = ["intruders-in-chetwood", "the-weather-hills", "deadmens-dike"]

pureIt :: String -> IO () -> Spec
pureIt = it

questOf :: CardDef -> GameT Quest
questOf def = getsGame (fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

damageOn :: CardDef -> GameT Int
damageOn def = getDamage . toTarget =<< questOf def

attachedTo :: CardDef -> GameT [Target]
attachedTo def =
  getsGame \g ->
    [t | a <- toList g.entities.attachments, toCardCode a == toCardCode def, AttachedTo t <- [attachmentPlacement (toAttrs a)]]
      <> [t | o <- toList g.entities.objectives, toCardCode o == toCardCode def, AttachedTo t <- [objectivePlacement (toAttrs o)]]

-- | An enemy's attack dealing damage to the character.
attackDamages :: Target -> GameT ()
attackDamages t = do
  eid <- EnemyId <$> getRandom
  run $ DoStep 1 (DealDamage (EnemyAttackSource eid) t 1)

handCodes :: PlayerId -> GameT [CardCode]
handCodes pid = map toCardCode . (.hand) <$> getPlayer pid

spec :: Spec
spec = describe "Angmar Awakened campaign" do
  describe "the campaign log" do
    pureIt "plays The Lost Realm's scenarios, then the cycle's" do
      nextScenario angmar `shouldBe` Just "intruders-in-chetwood"
      nextScenario (wonFirst lostRealm angmar) `shouldBe` Just "the-wastes-of-eriador"

    pureIt "refuses the Amarthiúl hero until The Battle of Carn Dûm" do
      seatHeroes [["01012", "01007", "10115"]] angmar `shouldBe` Left ["10115"]
      let carnDum = wonFirst (lostRealm <> ["the-wastes-of-eriador", "escape-from-mount-gram", "across-the-ettenmoors", "the-treachery-of-rhudaur"]) angmar
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "10115"]] carnDum `shouldBe` Right 0

    pureIt "lets heroes change freely at The Wastes of Eriador" do
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "01001"]] (wonFirst lostRealm angmar) `shouldBe` Right 0
      (.threatPenalty) <$> seatHeroes [["01012", "01007", "01001"]] angmar `shouldBe` Right 1

    pureIt "leaves the cards lost in Mount Gram out of every deck" do
      let l = record LostInMountGram ["01016"] angmar {campaignLogPool = [PoolCard "AAC176" (Just 0) Nothing]}
      seatDeck 0 l [("01016", 2), ("01013", 3)] `shouldBe` [("01013", 3), ("AAC176", 1)]

    pureIt "keeps notes and counts across updates" do
      let l = updateLog (AddToPool $ PoolCard "AAC160" Nothing Nothing) angmar
      (.pool) (updateLog (RemoveFromPool "AAC160") l) `shouldBe` []
      (.threatPenalty) (updateLog (AdjustThreatPenalty (-2)) angmar) `shouldBe` -2
      recordedCount ProtectTheInnocentDamage "AAC158" (recordCount ProtectTheInnocentDamage "AAC158" 3 angmar) `shouldBe` 3

  describe "Intruders in Chetwood" do
    it "attaches Iârion's Pendant to Iârion and offers resources for damage on Protect the Innocent" . campaignTestIn intrudersInChetwood angmar testDeck $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.iarion
      run $ DoStep 9 Setup
      iarion <- getsGame (fromJustNote "no Iârion" . find ((== toCardCode Objectives.iarion) . toCardCode) . toList . (.entities.allies))
      attachedTo Attachments.iarionsPendant `shouldReturn` [toTarget iarion.id]
      beravor <- getHeroWithCode "01012"
      chooseLabel "$label.protectTheInnocent.placeDamage" pid
      damageOn Quests.protectTheInnocent `shouldReturn` 4
      getHeroResources beravor.id `shouldReturn` 1

    it "notes the damage on Protect the Innocent once won" . campaignTestIn intrudersInChetwood angmar testDeck $ \_ -> do
      run . AddToStaging =<< genEncounterCard Quests.protectTheInnocent
      q <- questOf Quests.protectTheInnocent
      run $ PlaceTokens GameSource (toTarget q) Damage 3
      run $ GameOver Won
      recordedCount ProtectTheInnocentDamage (toCardCode Quests.protectTheInnocent) <$> getCampaignLog `shouldReturn` 3

  describe "Protect the Innocent" do
    it "takes damage from attacks, loses it to progress, and loses the game at 10" . campaignTestIn intrudersInChetwood angmar testDeck $ \_ -> do
      run . AddToStaging =<< genEncounterCard Quests.protectTheInnocent
      q <- questOf Quests.protectTheInnocent
      attackDamages . toTarget =<< getHeroWithCode "01012"
      damageOn Quests.protectTheInnocent `shouldReturn` 1
      run $ PlaceTokens GameSource (toTarget q) Damage 5
      run $ PlaceTokens GameSource (toTarget q) Progress 2
      damageOn Quests.protectTheInnocent `shouldReturn` 4
      run $ PlaceTokens GameSource (toTarget q) Damage 6
      getsGame (.outcome) `shouldReturn` Just Lost

  describe "in nightmare mode" do
    it "builds the nightmare deck and still shuffles the campaign's burdens in" . campaignTestIn (overAttrs (\a -> a {scenarioMode = NightmareMode}) theWeatherHills) angmar testDeck $ \_ -> do
      run $ DoStep 1 Setup
      run $ DoStep 2 Setup
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Quests.mysteriousOmens))

  describe "The Weather Hills" do
    it "shuffles Mysterious Omens into the encounter deck and the pool" . campaignTestIn theWeatherHills angmar testDeck $ \_ -> do
      run $ DoStep 2 Setup
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Quests.mysteriousOmens))
      map (.code) . (.pool) <$> getCampaignLog >>= (`shouldSatisfy` elem (toCardCode Quests.mysteriousOmens))

  describe "Mysterious Omens" do
    it "leaves the pool once defeated, and its boon joins it on the hero" . campaignTestIn theWeatherHills angmar {campaignLogPool = [PoolCard (toCardCode Quests.mysteriousOmens) Nothing Nothing]} testDeck $ \pid -> do
      run . AddToStaging =<< genEncounterCard Quests.mysteriousOmens
      q <- questOf Quests.mysteriousOmens
      run $ CompleteQuest q.id
      -- pass, then Cunning, then Hardy
      void $ answerQuestion pid 2
      eowyn <- getHeroWithCode "01007"
      chooseTarget pid eowyn.id
      attachedTo Attachments.hardy `shouldReturn` [toTarget eowyn.id]
      (.pool) <$> getCampaignLog `shouldReturn` [PoolCard (toCardCode Attachments.hardy) Nothing (Just "01007")]

    it "offers the second player only the boons the first did not take" . twoPlayerCampaignTestIn theWeatherHills angmar (coreDeck "tactics") $ \one two -> do
      run . AddToStaging =<< genEncounterCard Quests.mysteriousOmens
      q <- questOf Quests.mysteriousOmens
      run $ CompleteQuest q.id
      getQuestion two >>= (`shouldSatisfy` isNothing)
      -- pass, then Cunning
      void $ answerQuestion one 1
      eowyn <- getHeroWithCode "01007"
      chooseTarget one eowyn.id
      offered <- maybe [] questionChoices <$> getQuestion two
      length [() | TargetLabel {} <- offered] `shouldBe` 4

  describe "Deadmen's Dike" do
    it "attaches a random burden to Thaurdir as he enters play, for the pool" . campaignTestIn deadmensDike angmar testDeck $ \_ -> do
      run . AddToStaging =<< genEncounterCard Enemies.thaurdirDeadmensDike
      burdens <- filter ((`elem` map toCardCode [Objectives.thaurdirsLegacy, Objectives.thaurdirsMajesty, Objectives.thaurdirsSpite]) . (.code)) . (.pool) <$> getCampaignLog
      length burdens `shouldBe` 1

    it "adds Arnor Ravaged to the pool and leaves the choice of Pendant or Courage" . campaignTestIn deadmensDike angmar testDeck $ \_ -> do
      run $ GameOver Won
      l <- getCampaignLog
      map (.code) l.pool `shouldBe` [toCardCode Objectives.arnorRavaged]
      Map.member ChoosePendantOrCourage l.notes `shouldBe` True

  describe "The Wastes of Eriador" do
    let wastes = record ChoosePendantOrCourage [] $ addToPool (PoolCard (toCardCode Objectives.arnorRavaged) Nothing Nothing) $ recordCount ProtectTheInnocentDamage (toCardCode Quests.protectTheInnocent) 2 $ wonFirst lostRealm angmar
    it "sets up Arnor Ravaged, Determination and the choice of Pendant or Courage" . campaignTestIn theWastesOfEriador wastes testDeck $ \pid -> do
      run $ DoStep 9 Setup
      chooseLabel "$label.campaign.iarionsPendant" pid
      l <- getCampaignLog
      map (.code) l.pool `shouldSatisfy` \codes -> all (`elem` codes) [toCardCode Attachments.iarionsPendant, toCardCode Events.determination]
      Map.member ChoosePendantOrCourage l.notes `shouldBe` False
      handCodes pid >>= (`shouldSatisfy` elem (toCardCode Events.determination))
      ravaged <- getsGame (fromJustNote "no Arnor Ravaged" . find ((== toCardCode Objectives.arnorRavaged) . toCardCode) . toList . (.entities.objectives))
      getDamage (toTarget ravaged.id) `shouldReturn` 2

    it "has Fend Off Despair lower the threat penalty once defeated" . campaignTestIn theWastesOfEriador wastes testDeck $ \_ -> do
      run . AddToStaging =<< genEncounterCard Quests.fendOffDespair
      q <- questOf Quests.fendOffDespair
      run $ CompleteQuest q.id
      (.threatPenalty) <$> getCampaignLog `shouldReturn` -2

  describe "Arnor Ravaged" do
    it "moves 1 damage to a character an attack damages" . campaignTestIn theWastesOfEriador angmar testDeck $ \_ -> do
      card <- genEncounterCard Objectives.arnorRavaged
      run $ AddToStaging card
      run $ PlaceTokens GameSource (CardIdTarget card.id) Damage 2
      beravor <- toTarget <$> getHeroWithCode "01012"
      attackDamages beravor
      getDamage beravor `shouldReturn` 2

  describe "Across the Ettenmoors" do
    it "reveals Losing Time every third quest phase" . campaignTestIn acrossTheEttenmoors angmar testDeck $ \pid -> do
      g <- getGame
      putGame g {gameRound = 3}
      threat <- (.threat) <$> getPlayer pid
      run $ BeginPhase QuestPhase
      resolveRevealed
      chooseLabel "$label.losingTime.raiseThreat" pid
      (.threat) <$> getPlayer pid `shouldReturn` threat + 3

    it "adds each copy of Losing Time left in the encounter deck to the pool" . campaignTestIn acrossTheEttenmoors angmar testDeck $ \_ -> do
      _ <- setEncounterDeck [Treacheries.losingTime, Treacheries.losingTime]
      run $ GameOver Won
      map (.code) . (.pool) <$> getCampaignLog `shouldReturn` replicate 2 (toCardCode Treacheries.losingTime)

  describe "Escape from Mount Gram" do
    it "raises threat in the refresh phase by its campaign card's count" . campaignTestIn theWastesOfEriador angmar testDeck $ \pid -> do
      threat <- (.threat) <$> getPlayer pid
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [RefreshThreatRaise 3] UntilEndOfRound
      run $ BeginPhase RefreshPhase
      (.threat) <$> getPlayer pid `shouldReturn` threat + 3

  describe "The Treachery of Rhudaur" do
    it "adds the book of each Clue a player controls to the pool, for their deck" . campaignTestIn theTreacheryOfRhudaur angmar testDeck $ \_ -> do
      beravor <- getHeroWithCode "01012"
      run . (`PutObjectiveIntoPlayAttached` toTarget beravor.id) =<< genEncounterCard Objectives.heirloomOfIarchon
      run $ GameOver Won
      (.pool) <$> getCampaignLog `shouldReturn` [PoolCard (toCardCode Attachments.heirloomOfIarchon) (Just 0) Nothing]

  describe "The Battle of Carn Dûm and The Dread Realm" do
    it "notes the damage on each hero" . campaignTestIn theBattleOfCarnDum angmar testDeck $ \_ -> do
      eowyn <- getHeroWithCode "01007"
      run $ PlaceTokens GameSource (toTarget eowyn.id) Damage 2
      run $ GameOver Won
      recordedCount HeroDamage "01007" <$> getCampaignLog `shouldReturn` 2

    it "has each hero begin The Dread Realm with that damage" . campaignTestIn theDreadRealm (recordCount HeroDamage "01007" 2 angmar) testDeck $ \pid -> do
      run $ DoStep 9 Setup
      chooseLabel "$label.pass" pid
      eowyn <- getHeroWithCode "01007"
      getDamage (toTarget eowyn.id) `shouldReturn` 2
