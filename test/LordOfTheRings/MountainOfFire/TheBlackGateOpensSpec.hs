module LordOfTheRings.MountainOfFire.TheBlackGateOpensSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (..), CampaignLogKey (BlackGateResources), CompletedScenario (..), PoolCard (..), newCampaignLog, recordedCount)
import LordOfTheRings.Card (CardType (AllyType, HeroType), cdCardCode, cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getStagingThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithType), HeroMatcher (CommittedHero), PlayerMatcher (PlayerAtValourThreat), getEliminationLevel, select)
import LordOfTheRings.Message.Lifted (lowerThreatBy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theBlackGateOpens)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

gateTest :: (PlayerId -> GameT ()) -> IO ()
gateTest = gameTestIn theBlackGateOpens testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

beravor :: CardCode
beravor = "01012"

aragorn :: CardCode
aragorn = cdCardCode Heroes.aragornTheMountainOfFire

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

stagingCodes :: GameT [CardCode]
stagingCodes = getsGame \g ->
  [toCardCode e | e <- toList g.entities.enemies, e.placement == StagingArea]
    <> [toCardCode l | l <- toList g.entities.locations, l.placement == StagingArea]

offersLabel :: PlayerId -> Text -> GameT Bool
offersLabel pid l = maybe False (any \case Label l' _ -> l' == l; _ -> False) . fmap questionChoices <$> getQuestion pid

-- | The player's deck, from the top.
setDeck :: PlayerId -> [CardDef] -> GameT ()
setDeck pid defs = do
  cards <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) defs
  putGame . (playersL . ix pid . deckL .~ cards) =<< getGame

-- | Setup with stage 1A clicked, stage 1B resolved with nothing to pair and stage 2A with
-- nothing to find: the players at stage 2B, Gwaihir in the encounter deck.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  setDeck pid []
  _ <- setEncounterDeck []
  resolveWhenRevealed pid
  resolveWhenRevealed pid

blackGate :: GameT LocationId
blackGate = locationOf Locations.theBlackGate

spec :: Spec
spec = describe "The Black Gate Opens" do
  it "is a scenario the game can be dealt" . gateTest $ \_ ->
    isJust (lookupScenario "the-black-gate-opens") `shouldBe` True

  describe "setup" do
    it "builds the encounter deck from The Black Gate Opens and Orcs of Mordor, The Black Gate, The Mouth of Sauron and Gwaihir set aside" . gateTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.towersOfTheTeeth, Locations.beleagueredHills, Locations.wasteOfDagorlad, Locations.slopesOfCirithGorgor, Locations.reekingMoat, Locations.wallOfMorannon]
        `shouldBe` [2, 2, 3, 2, 3, 2]
      map (`copies` deck) [Enemies.nazgulOfMordor, Enemies.gorgorothHillTroll, Enemies.easterlingBowman, Enemies.warriorOfRhunTheBlackGateOpens, Enemies.orcOfMordor]
        `shouldBe` [3, 2, 3, 3, 3]
      map (`copies` deck) [Treacheries.hellHawk, Treacheries.theHourOfDoom, Treacheries.jawsOfSteel, Treacheries.rageFilledHim, Treacheries.onslaughtOfMordor]
        `shouldBe` [2, 3, 2, 3, 3]
      sort . map toCardCode . scenarioSetAside <$> scenarioAttrs
        `shouldReturn` sort (map toCardCode [Locations.theBlackGate, Enemies.theMouthOfSauron, Objectives.gwaihirTheBlackGateOpens])

    it "adds The Black Gate and The Mouth of Sauron to the staging area, the first player controlling Aragorn" . gateTest $ \pid -> do
      flipFirstQuest pid
      sort <$> stagingCodes `shouldReturn` sort (map toCardCode [Locations.theBlackGate, Enemies.theMouthOfSauron])
      map toCardCode . scenarioSetAside <$> scenarioAttrs `shouldReturn` [toCardCode Objectives.gwaihirTheBlackGateOpens]
      (.owner) <$> getHeroWithCode aragorn `shouldReturn` pid
      activeQuestCode `shouldReturn` toCardCode Quests.embassyFromTheDarkTower

  describe "Embassy from the Dark Tower" do
    it "pairs the highest cost player card with the highest threat encounter card, plays it as its pair is revealed and advances" . gateTest $ \pid -> do
      flipFirstQuest pid
      setDeck pid [Allies.snowbournScout, Allies.brokIronfist, Allies.guardOfTheCitadel, Allies.silverlodeArcher, Allies.gondorianSpearman]
      _ <- setEncounterDeck [Locations.reekingMoat, Locations.wasteOfDagorlad, Locations.wallOfMorannon, Locations.slopesOfCirithGorgor, Treacheries.jawsOfSteel]
      resolveWhenRevealed pid
      -- Brok Ironfist (6) with Wall of Morannon (5): revealed, Brok played for nothing
      chooseLabel "$label.embassyFromTheDarkTower.reveal" pid
      -- Silverlode Archer (3) with Slopes of Cirith Gorgor (4), and so on: discarded
      chooseLabel "$label.embassyFromTheDarkTower.discard" pid
      chooseLabel "$label.embassyFromTheDarkTower.discard" pid
      chooseLabel "$label.embassyFromTheDarkTower.discard" pid
      stagingCodes >>= (`shouldSatisfy` elem (toCardCode Locations.wallOfMorannon))
      allies <- getsGame (map toCardCode . toList . (.entities.allies))
      allies `shouldBe` [toCardCode Allies.brokIronfist]
      discarded <- map toCardCode . (.discard) <$> getPlayer pid
      sort discarded `shouldBe` sort (map toCardCode [Allies.snowbournScout, Allies.guardOfTheCitadel, Allies.silverlodeArcher])
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Treacheries.jawsOfSteel]
      sort . map toCardCode . scenarioEncounterDiscard <$> scenarioAttrs
        `shouldReturn` sort (map toCardCode [Locations.reekingMoat, Locations.wasteOfDagorlad, Locations.slopesOfCirithGorgor])
      map toCardCode . (.deck) <$> getPlayer pid `shouldReturn` [toCardCode Allies.gondorianSpearman]
      activeQuestCode `shouldReturn` toCardCode Quests.theBattleOfTheMorannon

  describe "The Battle of the Morannon" do
    it "has each player search for an Orc, Troll or Nazgûl enemy and shuffles Gwaihir into the encounter deck" . gateTest $ \pid -> do
      flipFirstQuest pid
      setDeck pid []
      _ <- setEncounterDeck []
      resolveWhenRevealed pid
      _ <- setEncounterDeck [Enemies.nazgulOfMordor, Enemies.easterlingBowman]
      activeQuestCode `shouldReturn` toCardCode Quests.theBattleOfTheMorannon
      resolveWhenRevealed pid
      nazgul <- getsGame (fromJustNote "no Nazgul" . find ((== toCardCode Enemies.nazgulOfMordor) . toCardCode) . (.encounterDeck) . toAttrs . (.scenario))
      chooseTarget pid (CardIdTarget nazgul.id)
      stagingCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.nazgulOfMordor))
      sort . map toCardCode <$> getEncounterDeck
        `shouldReturn` sort (map toCardCode [Enemies.easterlingBowman, Objectives.gwaihirTheBlackGateOpens])

    it "has the first player reveal 1 encounter card for each resource on The Black Gate, and discards the active location as the players quest unsuccessfully" . gateTest $ \pid -> do
      setUp pid
      gate <- blackGate
      run $ PlaceTokens GameSource (LocationTarget gate) Resource 3
      getModifiers GameTarget >>= (`shouldSatisfy` elem (FirstPlayerRevealsForStaging 3))
      moat <- stageLocation Locations.reekingMoat
      run $ PlaceLocation moat ActiveLocation
      run $ CheckWindows [mkAfter QuestedUnsuccessfully]
      getsGame (lookup moat . (.entities.locations)) >>= (`shouldSatisfy` isNothing)

  describe "The Black Gate" do
    it "takes a resource and raises each player's threat by 1 as the quest phase begins, and gets +1 threat for each" . gateTest $ \pid -> do
      setUp pid
      gate <- blackGate
      threat <- threatOf pid
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      threatOf pid `shouldReturn` (threat + 1)
      getLocationThreat gate `shouldReturn` 1
      getModifiers (LocationTarget gate) >>= (`shouldSatisfy` \ms -> ImmuneToPlayerCardEffects `elem` ms && CannotBeTraveledTo `elem` ms)

  describe "Dire" do
    it "raises the elimination level to 99, Valour to 80, and limits threat reduction by non-boon player cards to 1 each round" . gateTest $ \pid -> do
      setUp pid
      getEliminationLevel pid `shouldReturn` 99
      setThreat pid 60
      run CheckEliminations
      (.eliminated) <$> getPlayer pid `shouldReturn` False
      select PlayerAtValourThreat `shouldReturn` []
      setThreat pid 80
      select PlayerAtValourThreat `shouldReturn` [pid]
      b <- getHeroWithCode beravor
      runQueueT (lowerThreatBy (HeroSource $ toId b) pid 3) >> void mainLoop
      threatOf pid `shouldReturn` 79
      runQueueT (lowerThreatBy (HeroSource $ toId b) pid 3) >> void mainLoop
      threatOf pid `shouldReturn` 79
      -- an encounter card's reduction is not a player card's
      runQueueT (lowerThreatBy GameSource pid 3) >> void mainLoop
      threatOf pid `shouldReturn` 76

  describe "locations" do
    it "has Towers of the Teeth's threat be the most characters a player controls and bar allies while active" . gateTest $ \pid -> do
      setUp pid
      towers <- stageLocation Locations.towersOfTheTeeth
      characters <- length <$> getsGame (toList . (.entities.heroes))
      getLocationThreat towers `shouldReturn` characters
      run $ PlaceLocation towers ActiveLocation
      getModifiers GameTarget >>= (`shouldSatisfy` elem (CannotPlay (CardWithType AllyType)))

    it "has Beleaguered Hills engage an enemy from each player and take the engaged enemies' threat off the staging total" . gateTest $ \pid -> do
      setUp pid
      total <- getStagingThreat
      hills <- stageLocation Locations.beleagueredHills
      bowman <- stage Enemies.easterlingBowman
      run $ TravelTo hills
      placementOfEnemy bowman `shouldReturn` EngagedWith pid
      getStagingThreat `shouldReturn` (total - 2)

    it "has Reeking Moat take an enemy under it and put it into play engaged with the first player as it leaves" . gateTest $ \pid -> do
      setUp pid
      moat <- stageLocation Locations.reekingMoat
      _ <- stage Enemies.easterlingBowman
      run $ TravelTo moat
      getsGame (length . filter ((== toCardCode Enemies.easterlingBowman) . toCardCode) . toList . (.entities.enemies)) `shouldReturn` 0
      run $ Discard (LocationTarget moat)
      bowman <- enemyOf Enemies.easterlingBowman
      placementOfEnemy bowman `shouldReturn` EngagedWith pid

    it "has Slopes of Cirith Gorgor put an enemy from the encounter deck into play engaged with the first player as it leaves" . gateTest $ \pid -> do
      setUp pid
      slopes <- stageLocation Locations.slopesOfCirithGorgor
      _ <- setEncounterDeck [Treacheries.jawsOfSteel, Enemies.gorgorothHillTroll]
      run $ Discard (LocationTarget slopes)
      troll <- enemyOf Enemies.gorgorothHillTroll
      placementOfEnemy troll `shouldReturn` EngagedWith pid

    it "has Wall of Morannon return each engaged enemy to the staging area" . gateTest $ \pid -> do
      setUp pid
      wall <- stageLocation Locations.wallOfMorannon
      bowman <- stage Enemies.easterlingBowman
      run $ EngageEnemy pid bowman
      run $ TravelTo wall
      placementOfEnemy bowman `shouldReturn` StagingArea
      getModifiers (EnemyTarget bowman) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  describe "enemies" do
    it "has The Mouth of Sauron attack a player who cancels a when revealed effect" . gateTest $ \pid -> do
      setUp pid
      mouth <- enemyOf Enemies.theMouthOfSauron
      getModifiers (EnemyTarget mouth) >>= (`shouldSatisfy` elem CannotLeaveStagingArea)
      run $ CheckWindows [mkAfter $ WhenRevealedCanceled pid (EnemyTarget mouth)]
      -- the attack's action window, then its defender
      passActions pid
      Just question <- getQuestion pid
      [() | TargetLabel (HeroTarget _) _ <- questionChoices question] `shouldSatisfy` notNull

    it "has Nazgûl of Mordor raise the defending player's threat by 4 or return to the staging area after it attacks" . gateTest $ \pid -> do
      setUp pid
      nazgul <- stage Enemies.nazgulOfMordor
      run $ EngageEnemy pid nazgul
      threat <- threatOf pid
      run $ CheckWindows [mkAfter $ EnemyAttacking nazgul pid]
      chooseLabel "$label.nazgulOfMordor.threat" pid
      threatOf pid `shouldReturn` (threat + 4)
      run $ CheckWindows [mkAfter $ EnemyAttacking nazgul pid]
      chooseLabel "$label.nazgulOfMordor.return" pid
      placementOfEnemy nazgul `shouldReturn` StagingArea

    it "has Easterling Bowman deal 1 damage to a questing character of the revealer" . gateTest $ \pid -> do
      setUp pid
      b <- hero beravor
      run $ CommitToQuest b
      reveal [Enemies.easterlingBowman]
      getCharacterDamage b `shouldReturn` 1

    it "has Warrior of Rhûn remove a questing character from the quest" . gateTest $ \pid -> do
      setUp pid
      b <- hero beravor
      run $ CommitToQuest b
      reveal [Enemies.warriorOfRhunTheBlackGateOpens]
      select CommittedHero `shouldReturn` []
      _ <- pure pid
      pure ()

  describe "treacheries" do
    it "has Hell-hawk attach to a Nazgûl enemy, which gets +2 threat, attack and defense and immunity" . gateTest $ \pid -> do
      setUp pid
      nazgul <- stage Enemies.nazgulOfMordor
      -- with no Nazgûl to search for, it attaches to the one in play
      reveal [Treacheries.hellHawk]
      _ <- pure pid
      getModifiers (EnemyTarget nazgul) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ThreatModifier 2, AttackModifier 2, DefenseModifier 2, ImmuneToPlayerCardEffects])

    it "has The Hour of Doom raise each threat by 1 for each character with no ally to discard" . gateTest $ \pid -> do
      setUp pid
      characters <- length <$> getsGame (toList . (.entities.heroes))
      threat <- threatOf pid
      reveal [Treacheries.theHourOfDoom, Locations.reekingMoat]
      -- Doomed 1, then 1 for each character
      threatOf pid `shouldReturn` (threat + 1 + characters)

    it "has Jaws of Steel deal X damage, X the resources on The Black Gate" . gateTest $ \pid -> do
      setUp pid
      gate <- blackGate
      run $ PlaceTokens GameSource (LocationTarget gate) Resource 2
      b <- hero beravor
      reveal [Treacheries.jawsOfSteel]
      chooseTarget pid b
      chooseTarget pid b
      getCharacterDamage b `shouldReturn` 2

    it "has Onslaught of Mordor damage a questing character, which then gets -1 willpower and cannot be healed" . gateTest $ \pid -> do
      setUp pid
      b <- hero beravor
      run $ CommitToQuest b
      reveal [Treacheries.onslaughtOfMordor]
      getCharacterDamage b `shouldReturn` 1
      getModifiers b >>= (`shouldSatisfy` \ms -> WillpowerModifier (-1) `elem` ms && CannotBeHealed `elem` ms)

  describe "Gwaihir" do
    it "joins the first player as an ally as he is revealed" . gateTest $ \pid -> do
      setUp pid
      -- with no enemy to discard, he joins the first player
      reveal [Objectives.gwaihirTheBlackGateOpens]
      allies <- getsGame (map toCardCode . filter ((== InPlayArea pid) . (.placement)) . toList . (.entities.allies))
      allies `shouldBe` [toCardCode Objectives.gwaihirTheBlackGateOpens]

    it "may instead be discarded for a Fellowship resource to discard a non-unique enemy in the staging area" . gateTest $ \pid -> do
      setUp pid
      a <- hero aragorn
      run $ PlaceTokens GameSource a Resource 1
      bowman <- stage Enemies.easterlingBowman
      reveal [Objectives.gwaihirTheBlackGateOpens]
      offersLabel pid "$label.gwaihirTheBlackGateOpens.control" `shouldReturn` True
      chooseLabel "$label.gwaihirTheBlackGateOpens.discard" pid
      getsGame (lookup bowman . (.entities.enemies)) >>= (`shouldSatisfy` isNothing)
      (getHeroResources . toId =<< getHeroWithCode aragorn) `shouldReturn` 0
      getsGame (length . toList . (.entities.allies)) `shouldReturn` 0

  describe "campaign mode" do
    let campaignLog = (newCampaignLog "the-lord-of-the-rings") {campaignLogPool = [PoolCard wraith (Just 0) Nothing, PoolCard wraith (Just 0) Nothing]}
        campaignTest = campaignTestIn theBlackGateOpens campaignLog testDeck
        wraith = cdCardCode Enemies.wraithOnWings

    it "lets the first player raise each threat by 1 to search his deck for Andúril" . campaignTest $ \pid -> do
      anduril <- genPlayerCard Attachments.andurilTheRoadDarkens (Just pid)
      putGame . (playersL . ix pid . deckL %~ (<> [PlayerCard anduril])) =<< getGame
      threat' <- threatOf pid
      run $ DoStep 7 Setup
      chooseLabel "$label.searchForAnduril.search" pid
      chooseTarget pid (CardIdTarget anduril.id)
      threatOf pid `shouldReturn` (threat' + 1)
      map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Attachments.andurilTheRoadDarkens))

    it "resolves once lost: the resources on The Black Gate, a Wraith on Wings defeated, Gwaihir earned and heroes in discard piles not fallen" . campaignTest $ \pid -> do
      gate <- stageLocation Locations.theBlackGate
      run $ PlaceTokens GameSource (LocationTarget gate) Resource 4
      gwaihir <- genEncounterCard Objectives.gwaihirTheBlackGateOpens
      run $ RevealCard gwaihir
      resolveRevealed
      defeated <- genEncounterCard Enemies.wraithOnWings
      putGame . (scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard defeated])) =<< getGame
      run . Discard =<< hero beravor
      run $ GameOver Lost
      l <- getCampaignLog
      map (.completedScenario) l.completed `shouldBe` ["the-black-gate-opens"]
      recordedCount BlackGateResources (toCardCode Locations.theBlackGate) l `shouldBe` 4
      map (.code) l.pool `shouldBe` [wraith, toCardCode Objectives.gwaihirTheBlackGateOpens]
      l.fallenHeroes `shouldBe` []
      heroes <- map toCardCode . filter ((== HeroType) . cdCardType . toCardDef) . (.discard) <$> getPlayer pid
      heroes `shouldBe` [beravor]
