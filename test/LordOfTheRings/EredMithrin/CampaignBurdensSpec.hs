module LordOfTheRings.EredMithrin.CampaignBurdensSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (campaignLogPool), PoolCard (..), newCampaignLog)
import LordOfTheRings.Card (cdCardCode, cdUnique, isTreasure)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Matcher (EnemyMatcher (UniqueEnemy), PlayerMatcher (NotPlayer, PlayerEngagedWith))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioCampaign, scenarioGuardedPlayerCards, scenarioSetAside), encounterDiscardL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Trait (Trait (Dragon))
import LordOfTheRings.Window (WindowType (PhaseBegins), mkAfter)
import TestImport

beravorCode, eowynCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

inCampaign :: GameT ()
inCampaign = putGame . (scenarioL %~ overAttrs (\a -> a {scenarioCampaign = Just (newCampaignLog "ered-mithrin")})) =<< getGame

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` existing) . map toId . toList . (.entities.enemies))

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "Ered Mithrin campaign burdens" do
  describe "A Growing Threat" do
    it "sets aside the topmost eligible discarded card, adds a copy to the campaign pool during quest resolution, and leaves the game" . gameTest $ \_ -> do
      inCampaign
      _ <- setEncounterDeck [Treacheries.aGrowingThreat, Locations.oldForestRoad]
      spider <- genEncounterCard Enemies.forestSpider
      ufthak <- genEncounterCard Objectives.oldLantern
      putGame . (scenarioL %~ overAttrs (encounterDiscardL .~ [ufthak, spider])) =<< getGame
      run RevealEncounterCard
      resolveRevealed
      getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) >>= (`shouldSatisfy` elem (toCardCode Enemies.forestSpider))
      _ <- pure spider
      -- not yet: the copy waits for quest resolution
      map poolCardCode . campaignLogPool <$> getCampaignLog `shouldReturn` []
      run QuestResolution
      map poolCardCode . campaignLogPool <$> getCampaignLog `shouldReturn` [toCardCode Enemies.forestSpider]
      -- spent: a later quest resolution adds no other copy
      run QuestResolution
      map poolCardCode . campaignLogPool <$> getCampaignLog `shouldReturn` [toCardCode Enemies.forestSpider]
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.aGrowingThreat))

  describe "The Threat to Wilderland" do
    it "loses a resource as the quest phase begins, and may make every encounter card immune to player cards" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.theThreatToWilderland
      threatObj <- getsGame (toTarget . fromJustNote "no objective" . headMay . toList . (.entities.objectives))
      run $ PlaceTokens GameSource threatObj Resource 3
      eid <- stagedEnemy Enemies.forestSpider
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ CheckWindows [mkAfter (PhaseBegins QuestPhase)]
      chooseLabel "$label.theThreatToWilderland.immune" pid
      getModifiers (toTarget eid) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      threat pid `shouldReturn` 0

  describe "Dagnir's Spawn" do
    it "cannot have non-Dragon attachments, and is shuffled back once every player distributes its damage" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.dagnirsSpawn, Locations.oldForestRoad]
      run RevealEncounterCard
      chooseLabel "$label.dagnirsSpawn.deal" pid
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Enemies.dagnirsSpawn))
      eid <- stagedEnemy Enemies.dagnirsSpawn
      getModifiers (toTarget eid) >>= (`shouldSatisfy` elem (CannotHaveAttachmentsWithoutTrait Dragon))

  describe "Abandoned Ruin" do
    it "guards one of 3 random undiscovered treasures as it becomes the active location" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.abandonedRuin
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      ruin <- getLocation lid
      run $ TravelTo lid
      chooseAbilityOf pid (toTarget lid)
      q <- getQuestion pid
      let options = maybe [] (\case QuestionLabel _ _ _ (SearchCards _ (ChooseOne cs)) -> cs; _ -> []) q
      length options `shouldBe` 3
      case options of
        TargetLabel t _ : _ -> chooseTarget pid t
        _ -> error "no treasure offered"
      guarded <- getsGame (Map.findWithDefault [] (toAttrs ruin).cardId . scenarioGuardedPlayerCards . toAttrs . (.scenario))
      map (isTreasure . toCardCode . snd) guarded `shouldBe` [True]

  describe "Urdug's Cunning" do
    it "has each player deal 1 damage for each enemy among the top 5 cards it discards" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- setEncounterDeck [Treacheries.urdugsCunning, Enemies.forestSpider, Enemies.forestSpider, Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid beravor
      chooseTarget pid beravor
      getCharacterDamage beravor `shouldReturn` 2

  describe "Misplaced Trust" do
    it "attaches itself to a hero its player controls as it is drawn, making that hero immune to player cards" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      trust <- EncounterCard <$> genEncounterCard Treacheries.misplacedTrust
      putGame . (playersL . ix pid . deckL .~ [trust]) =<< getGame
      run $ DrawCards (DrawingFrom pid 1 GameSource)
      chooseTarget pid beravor
      getModifiers beravor >>= (`shouldSatisfy` \ms -> ImmuneToPlayerCardEffects `elem` ms && CannotHavePlayerAttachments `elem` ms)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` notElem trust.id)

    it "leaves play to its owner's discard pile, not the encounter discard pile" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      trust <- EncounterCard <$> genEncounterCard Treacheries.misplacedTrust
      putGame . (playersL . ix pid . deckL .~ [trust]) =<< getGame
      run $ DrawCards (DrawingFrom pid 1 GameSource)
      chooseTarget pid beravor
      tid <- getsGame (toId . fromJustNote "not attached" . headMay . toList . (.entities.treacheries))
      run $ Discard (toTarget tid)
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [trust.id]
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.misplacedTrust))

  describe "Gundabad Stalker" do
    it "cannot be optionally engaged but by a player engaged with a unique enemy" . gameTest $ \_ -> do
      eid <- stagedEnemy Enemies.gundabadStalker
      getModifiers (toTarget eid) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy (NotPlayer $ PlayerEngagedWith UniqueEnemy)))

  describe "Gundabad Hunter" do
    -- Chieftain Ufthak is unique (Core Set 90): the second player, engaged with him, is
    -- the only one to choose, not the first, engaged with a Forest Spider
    it "attacks the player engaged with a unique enemy as it is revealed" . twoPlayerTest testDeck $ \p1 p2 -> do
      cdUnique Enemies.chieftanUfthak `shouldBe` True
      ufthak <- stagedEnemy Enemies.chieftanUfthak
      run $ EngageEnemy p2 ufthak
      spider <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy p1 spider
      _ <- setEncounterDeck [Enemies.gundabadHunter, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      -- no player is chosen: the attack is against the second player, who is asked to
      -- declare a defender
      let labelled prefix = \case
            Just (QuestionLabel l _ _ _) -> prefix `isPrefixOf` l
            _ -> False
      getQuestion p1 >>= (`shouldSatisfy` not . labelled "$label.choosePlayer")
      -- past the action windows, the second player, not the first, declares a defender
      replicateM_ 3 (passActions p1 >> passActions p2)
      getQuestion p1 >>= (`shouldSatisfy` isNothing)
      getQuestion p2 >>= (`shouldSatisfy` isJust)

  describe "Dragon-sickness" do
    it "attaches to a character with a treasure, gathers every treasure there, and raises threat as it exhausts" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.jewelsOfWilderland beravor
      _ <- putAttachmentOn pid Attachments.helmOfHeroes eowyn
      _ <- setEncounterDeck [Treacheries.dragonSickness, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid beravor
      getsGame (map (.placement) . filter isTreasure . toList . (.entities.attachments)) `shouldReturn` [AttachedTo beravor, AttachedTo beravor]
      was <- threat pid
      run $ Exhaust beravor
      threat pid `shouldReturn` was + 2

  describe "Ashen Brazier" do
    it "is a loot boon that exhausts to look at a facedown shadow card" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.ashenBrazier
      brazier <- getsGame (toTarget . fromJustNote "no objective" . headMay . toList . (.entities.objectives))
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ActionWindow pid
      chooseAbilityOf pid brazier
      offersAbilityOf pid brazier `shouldReturn` False
      cdCardCode Objectives.ashenBrazier `shouldBe` "EMC190"
