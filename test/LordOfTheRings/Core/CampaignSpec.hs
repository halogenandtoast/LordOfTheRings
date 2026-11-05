module LordOfTheRings.Core.CampaignSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally (Ally)
import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), scenarioCampaignCardFlipped, scenarioMeta, scenarioMode)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

core :: CampaignLog
core = newCampaignLog "revised-core"

-- | The starter deck's heroes, as the first scenario records them.
seated :: CampaignLog
seated = core {campaignLogHeroes = [["01001", "01002", "01003"]]}

mendors :: GameT [Ally]
mendors = getsGame (filter ((== toCardCode Objectives.mendor) . toCardCode) . toList . (.entities.allies))

attachedTo :: CardDef -> GameT [Target]
attachedTo def =
  getsGame \g ->
    [t | a <- toList g.entities.attachments, toCardCode a == toCardCode def, AttachedTo t <- [attachmentPlacement (toAttrs a)]]
      <> [t | o <- toList g.entities.objectives, toCardCode o == toCardCode def, AttachedTo t <- [objectivePlacement (toAttrs o)]]

setMeta :: Value -> GameT ()
setMeta v = putGame . (scenarioL %~ overAttrs (\a -> a {scenarioMeta = v})) =<< getGame

-- | A test of pure values, in IO.
pureIt :: String -> IO () -> Spec
pureIt = it

spec :: Spec
spec = describe "Campaign mode" do
  describe "the campaign log" do
    pureIt "records the heroes of the first scenario at no penalty" do
      seatHeroes [["01001", "01002", "01003"]] core `shouldBe` Right seated

    pureIt "adds 1 to the threat penalty for each hero changed" do
      (.threatPenalty) <$> seatHeroes [["01001", "01002", "01004"]] seated `shouldBe` Right 1

    pureIt "refuses a fallen hero" do
      seatHeroes [["01001", "01002", "01003"]] (seated {campaignLogFallenHeroes = ["01003"]}) `shouldBe` Left ["01003"]

    pureIt "drops the Permanent cards of a fallen hero from the pool" do
      let l = seated {campaignLogPool = [PoolCard "RC007" Nothing (Just "01003"), PoolCard "RC005" Nothing Nothing]}
      (.pool) (recordFallen ["01003"] l) `shouldBe` [PoolCard "RC005" Nothing Nothing]

    pureIt "plays the scenarios in order" do
      nextScenario core `shouldBe` Just "passage-through-mirkwood"
      nextScenario seated {campaignLogCompleted = [CompletedScenario "passage-through-mirkwood" 100]} `shouldBe` Just "journey-along-the-anduin"

  describe "setting up" do
    it "adds the threat penalty to each player's starting threat" . campaignTestIn passageThroughMirkwood seated {campaignLogThreatPenalty = 2} starterDeck $ \pid -> do
      heroes <- getsGame (sum . map (.threat) . toList . (.entities.heroes))
      run $ DoStep 2 Setup
      (.threat) <$> getPlayer pid `shouldReturn` heroes + 2

    it "shuffles the Campaign Pool's encounter cards into the encounter deck" . campaignTestIn passageThroughMirkwood seated {campaignLogPool = [PoolCard (toCardCode Treacheries.lingeringVenom) Nothing Nothing]} starterDeck $ \_ -> do
      run $ DoStep 2 Setup
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Treacheries.lingeringVenom))

    it "attaches the Permanent boons and burdens to the heroes the log records" . campaignTestIn passageThroughMirkwood seated {campaignLogPool = [PoolCard (toCardCode Attachments.valor) Nothing (Just "01002"), PoolCard (toCardCode Objectives.scarred) Nothing (Just "01003")]} starterDeck $ \_ -> do
      run $ DoStep 4 Setup
      theodred <- toTarget <$> getHeroWithCode "01002"
      gloin <- toTarget <$> getHeroWithCode "01003"
      attachedTo Attachments.valor `shouldReturn` [theodred]
      attachedTo Objectives.scarred `shouldReturn` [gloin]

  describe "Scarred" do
    it "raises the threat of the hero's controller for a character they control destroyed, an objective-ally too" . campaignTestIn passageThroughMirkwood seated starterDeck $ \pid -> do
      gloin <- getHeroWithCode "01003"
      run . (`PutObjectiveIntoPlayAttached` toTarget gloin) =<< genEncounterCard Objectives.scarred
      run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.calaerion
      calaerion <- getsGame (fromJustNote "no Calaerion" . headMay . toList . (.entities.allies))
      threat <- (.threat) <$> getPlayer pid
      run $ Destroy GameSource (toTarget calaerion.id)
      (.threat) <$> getPlayer pid `shouldReturn` threat + 1

  describe "Passage Through Mirkwood" do
    it "puts Mendor into play under the first player" . campaignTestIn passageThroughMirkwood seated starterDeck $ \pid -> do
      run $ DoStep 4 Setup
      map (.placement) <$> mendors `shouldReturn` [InPlayArea pid]

    it "is lost if Mendor leaves play" . campaignTestIn passageThroughMirkwood seated starterDeck $ \_ -> do
      run $ DoStep 4 Setup
      mendor <- fromJustNote "no Mendor" . headMay <$> mendors
      run $ Discard GameSource (toTarget mendor.id)
      getsGame (.outcome) `shouldReturn` Just Lost

    it "earns Mendor's Support and the burden of the path taken" . campaignTestIn passageThroughMirkwood seated starterDeck $ \_ -> do
      setMeta $ toJSON $ toCardCode Quests.aChosenPathBeornsPath
      run $ GameOver Won
      l <- getCampaignLog
      l.pool
        `shouldBe` [ PoolCard (toCardCode Events.mendorsSupport) (Just 0) Nothing
                   , PoolCard (toCardCode Enemies.ungoliantsSwarm) Nothing Nothing
                   ]
      map completedScenario l.completed `shouldBe` ["passage-through-mirkwood"]
      getsGame (scenarioCampaignCardFlipped . toAttrs . (.scenario)) `shouldReturn` True

    it "takes Lingering Venom after Don't Leave the Path" . campaignTestIn passageThroughMirkwood seated starterDeck $ \_ -> do
      setMeta $ toJSON $ toCardCode Quests.aChosenPathDontLeaveThePath
      run $ GameOver Won
      map (.code) . (.pool) <$> getCampaignLog >>= (`shouldSatisfy` elem (toCardCode Treacheries.lingeringVenom))

    it "records a hero in its owner's discard pile as fallen, and scores the game" . campaignTestIn passageThroughMirkwood seated starterDeck $ \pid -> do
      gloin <- getHeroWithCode "01003"
      run $ Discard GameSource (toTarget gloin.id)
      threat <- (.threat) <$> getPlayer pid
      run $ GameOver Won
      l <- getCampaignLog
      l.fallenHeroes `shouldBe` ["01003"]
      -- the final threat, the fallen hero's threat cost, no damage, no rounds, no victory points
      map completedScore l.completed `shouldBe` [threat + 9]

    it "records nothing when lost" . campaignTestIn passageThroughMirkwood seated starterDeck $ \_ -> do
      run $ GameOver Lost
      getCampaignLog `shouldReturn` seated

  describe "Journey Along the Anduin" do
    it "has each player attach Valor once a Hill Troll is defeated" . campaignTestIn journeyAlongTheAnduin seated starterDeck $ \pid -> do
      troll <- genEncounterCard Enemies.hillTroll
      run $ CheckWindows [mkAfter $ EnemyDestroyed (EncounterCard troll)]
      theodred <- getHeroWithCode "01002"
      chooseTarget pid theodred.id
      attachedTo Attachments.valor `shouldReturn` [toTarget theodred.id]

    it "adds Valor and Scarred in play to the pool and names the most damaged hero prisoner" . campaignTestIn journeyAlongTheAnduin seated starterDeck $ \pid -> do
      theodred <- getHeroWithCode "01002"
      _ <- putAttachmentOn pid Attachments.valor (toTarget theodred.id)
      gloin <- getHeroWithCode "01003"
      run $ PlaceTokens GameSource (toTarget gloin.id) Damage 2
      run $ GameOver Won
      l <- getCampaignLog
      l.pool `shouldBe` [PoolCard (toCardCode Attachments.valor) Nothing (Just "01002")]
      recorded Prisoner l `shouldBe` ["01003"]

  describe "Escape from Dol Guldur" do
    let prisoner = seated {campaignLogNotes = Map.fromList [(Prisoner, ["01002"])]}
    it "takes the prisoner from the log, and Mendor with him" . campaignTestIn escapeFromDolGuldur prisoner starterDeck $ \pid -> do
      theodred <- getHeroWithCode "01002"
      run $ DoStep 4 Setup
      -- Appointed by Fate, on a hero of the first player's choice
      chooseTarget pid theodred.id
      attachedTo Attachments.appointedByFate `shouldReturn` [toTarget theodred.id]
      _ <- putQuestStage Quests.theNecromancersTower
      run . WhenRevealed . toTarget =<< getActiveQuest
      heroPrisoner . toAttrs <$> getHeroWithCode "01002" `shouldReturn` True
      map (.placement) <$> mendors `shouldReturn` [AttachedTo (toTarget theodred.id)]
      getsGame (scenarioCampaignCardFlipped . toAttrs . (.scenario)) `shouldReturn` True

    it "takes the named prisoner in nightmare mode too" . campaignTestIn (overAttrs (\a -> a {scenarioMode = NightmareMode}) escapeFromDolGuldur) prisoner starterDeck $ \_ -> do
      _ <- putQuestStage Quests.theNecromancersTower
      run . WhenRevealed . toTarget =<< getActiveQuest
      getsGame (map toCardCode . filter (heroPrisoner . toAttrs) . toList . (.entities.heroes)) `shouldReturn` ["01002"]
