module LordOfTheRings.OnTheDoorstep.TheBattleOfFiveArmiesSpec (spec) where

import LordOfTheRings.Card (cdCardCode, cdEncounterSet, lookupCardDef, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (TheBattleOfFiveArmies, Wilderland))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (questingStat)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (FirstPlayer), getCurrentQuest)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theBattleOfFiveArmies)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

bofaTest :: (PlayerId -> GameT ()) -> IO ()
bofaTest = gameTestIn theBattleOfFiveArmies testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup, Bolg and Northern Slopes the encounter deck stage 1A searches and reveals.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseLabel "$label.keepHand" pid
  _ <- setEncounterDeck [Enemies.bolg, Locations.northernSlopes]
  chooseOptionMatching "perform stage 1A's setup" pid (const True)
  resolveRevealed

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

beravor :: GameT Target
beravor = hero "01012"

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

questCodes :: GameT [CardCode]
questCodes = getsGame (sort . map toCardCode . toList . (.entities.quests))

stageOf :: CardDef -> GameT Target
stageOf def = getsGame (toTarget . fromJustNote "no such stage" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

progressOn :: CardDef -> GameT Int
progressOn def = getsGame (maybe 0 (.progress) . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

-- | Stage 1B turned over: stages 2 to 4 in play, stage 5 left in the quest deck.
toBattle :: GameT ()
toBattle = do
  cards <- traverse genEncounterCard [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill, Quests.theCloudsBurst]
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)
  run . PutQuestIntoPlay =<< genEncounterCard Quests.theGatheringOfTheClouds
  q <- stageOf Quests.theGatheringOfTheClouds
  run $ Flip q
  resolveRevealed

-- | The first player chooses the stage as the round begins.
chooseStage :: PlayerId -> CardDef -> GameT ()
chooseStage pid def = do
  run $ CheckWindows [mkWhen RoundBegins]
  chooseTarget pid =<< stageOf def

-- | Progress placed on the stage, as the players would quest there.
progressStage :: PlayerId -> CardDef -> Int -> GameT ()
progressStage pid def n = do
  chooseStage pid def
  run $ PlaceProgressOnQuest n

-- | Answer each question asked of the player with its first choice, until none is open.
answerAll :: PlayerId -> GameT ()
answerAll pid = replicateM_ 10 $ getQuestion pid >>= traverse_ (const $ chooseOptionMatching "answer" pid (const True))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

battleStageCodes :: [CardCode]
battleStageCodes = sort $ map cdCardCode [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill]

spec :: Spec
spec = describe "The Battle of Five Armies" do
  it "is a scenario the game can be dealt" . bofaTest $ \_ ->
    isJust (lookupScenario "the-battle-of-five-armies") `shouldBe` True

  describe "setup" do
    it "builds the encounter deck from The Battle of Five Armies and Wilderland" . bofaTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 44
      all ((`elem` map Just [TheBattleOfFiveArmies, Wilderland]) . cdEncounterSet . toCardDef) deck `shouldBe` True

    it "adds Bolg to the staging area, reveals 1 card per player, and puts stages 2 to 4 in play" . bofaTest $ \pid -> do
      setUp pid
      b <- getHeroWithCode "132001"
      b.owner `shouldBe` pid
      enemyCodes `shouldReturn` [toCardCode Enemies.bolg]
      _ <- locationOf Locations.northernSlopes
      questCodes `shouldReturn` battleStageCodes
      map toCardCode . (.questDeck) <$> scenarioAttrs `shouldReturn` [toCardCode Quests.theCloudsBurst]
      -- the first round begins with the first player choosing the current stage
      run BeginRound
      chooseTarget pid =<< stageOf Quests.battleInTheDale
      (getCurrentQuest `shouldReturn`) . Just =<< stageOf Quests.battleInTheDale

  describe "the current quest stage" do
    it "places progress only on the stage chosen for the round, questing with its keyword" . bofaTest $ \pid -> do
      toBattle
      chooseStage pid Quests.battleInTheDale
      fst <$> questingStat `shouldReturn` "attack"
      spur <- stageOf Quests.theEasternSpur
      getModifiers spur >>= (`shouldSatisfy` elem (ProgressLimit 0))
      run $ PlaceProgressOnQuest 3
      progressOn Quests.battleInTheDale `shouldReturn` 3
      progressOn Quests.theEasternSpur `shouldReturn` 0
      run $ PlaceTokens GameSource spur Progress 2
      progressOn Quests.theEasternSpur `shouldReturn` 0
      chooseStage pid Quests.theDefenseOfRavenhill
      fst <$> questingStat `shouldReturn` "defense"

    it "advances to stage 5 once all three stages are complete, which brings a Bodyguard of Bolg" . bofaTest $ \pid -> do
      toBattle
      _ <- setEncounterDeck [Enemies.bodyguardOfBolg]
      progressStage pid Quests.theEasternSpur 9
      progressStage pid Quests.battleInTheDale 8
      questCodes `shouldReturn` battleStageCodes
      progressStage pid Quests.theDefenseOfRavenhill 8
      resolveRevealed
      questCodes `shouldReturn` [toCardCode Quests.theCloudsBurst]
      -- Each player searches for a copy of Bodyguard of Bolg
      chooseOptionMatching "take the Bodyguard of Bolg" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      enemyCodes `shouldReturn` [toCardCode Enemies.bodyguardOfBolg]

    it "does not advance while a stage completed earlier has lost its progress" . bofaTest $ \pid -> do
      toBattle
      progressStage pid Quests.theEasternSpur 8
      progressStage pid Quests.battleInTheDale 8
      spur <- stageOf Quests.theEasternSpur
      run $ RemoveTokens GameSource spur Progress 1
      progressStage pid Quests.theDefenseOfRavenhill 8
      questCodes `shouldReturn` battleStageCodes

    it "has stage 2 make each player discard a random card at the end of a round it has no progress" . bofaTest $ \pid -> do
      toBattle
      _ <- putCardIntoHand pid (fromJustNote "no card" $ lookupCardDef "01013")
      hand <- length . (.hand) <$> getPlayer pid
      progressStage pid Quests.battleInTheDale 1
      progressStage pid Quests.theDefenseOfRavenhill 1
      run $ CheckWindows [mkAfter RoundEnds]
      length . (.hand) <$> getPlayer pid `shouldReturn` (hand - 1)

    it "has stage 3 add the topmost enemy of the discard pile to the staging area at the end of a round it has no progress" . bofaTest $ \pid -> do
      toBattle
      progressStage pid Quests.theEasternSpur 1
      progressStage pid Quests.theDefenseOfRavenhill 1
      eid <- stage Enemies.gundabadClimber
      run $ Discard GameSource (EnemyTarget eid)
      run $ CheckWindows [mkAfter RoundEnds]
      enemyCodes `shouldReturn` [toCardCode Enemies.gundabadClimber]

    it "has stage 4 empty each hero's resource pool at the end of a round it has no progress" . bofaTest $ \pid -> do
      toBattle
      progressStage pid Quests.theEasternSpur 1
      progressStage pid Quests.battleInTheDale 1
      b <- getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget b) Resource 3
      run $ CheckWindows [mkAfter RoundEnds]
      getHeroResources (toId b) `shouldReturn` 0

    it "wins the game once Bolg is destroyed at stage 5, where he is engaged with the first player" . bofaTest $ \_ -> do
      bolg <- stage Enemies.bolg
      _ <- setEncounterDeck []
      run . PutQuestIntoPlay =<< genEncounterCard Quests.theCloudsBurst
      q <- stageOf Quests.theCloudsBurst
      run $ Flip q
      resolveRevealed
      chooseLabel "$label.continue" =<< getLead
      getModifiers q >>= (`shouldSatisfy` elem (ProgressLimit 0))
      getModifiers (EnemyTarget bolg) >>= (`shouldSatisfy` elem (EngagedWithEachPlayer FirstPlayer))
      run $ Destroy GameSource (EnemyTarget bolg)
      getsGame (.outcome) `shouldReturn` Just Won

  describe "the enemies" do
    it "makes Bolg's threat the number of players, immune, and unharmed while a Bodyguard is in play" . bofaTest $ \_ -> do
      bolg <- stage Enemies.bolg
      getEnemyThreat bolg `shouldReturn` 1
      getModifiers (EnemyTarget bolg) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, CannotLeaveStagingArea])
      getModifiers (EnemyTarget bolg) >>= (`shouldSatisfy` notElem CannotTakeDamage)
      _ <- stage Enemies.bodyguardOfBolg
      getModifiers (EnemyTarget bolg) >>= (`shouldSatisfy` elem CannotTakeDamage)

    it "has Bolg give the first Goblin revealed each round surge" . bofaTest $ \_ -> do
      _ <- stage Enemies.bolg
      reveal [Enemies.gundabadWolfRider, Locations.northernSlopes, Enemies.gundabadWolfRider, Locations.northernSlopes]
      sort <$> enemyCodes `shouldReturn` map toCardCode [Enemies.bolg, Enemies.gundabadWolfRider]
      _ <- locationOf Locations.northernSlopes
      rider <- enemyOf Enemies.gundabadWolfRider
      getModifiers (EnemyTarget rider) >>= (`shouldSatisfy` elem (GainKeyword Surge))

    it "gives Vanguard of Bolg +1 threat, attack and defense for each stage without progress" . bofaTest $ \pid -> do
      toBattle
      vanguard <- stage Enemies.vanguardOfBolg
      getEnemyThreat vanguard `shouldReturn` 4
      getEnemyAttack vanguard `shouldReturn` 4
      progressStage pid Quests.theEasternSpur 1
      getEnemyThreat vanguard `shouldReturn` 3

    it "has Gundabad Climber remove 1 progress from every stage in play" . bofaTest $ \pid -> do
      toBattle
      progressStage pid Quests.theEasternSpur 3
      progressStage pid Quests.battleInTheDale 2
      reveal [Enemies.gundabadClimber]
      progressOn Quests.theEasternSpur `shouldReturn` 2
      progressOn Quests.battleInTheDale `shouldReturn` 1
      progressOn Quests.theDefenseOfRavenhill `shouldReturn` 0

    it "has Gundabad Wolf Rider remove 1 progress from the current quest as it attacks" . bofaTest $ \pid -> do
      toBattle
      progressStage pid Quests.theEasternSpur 3
      rider <- engaged pid Enemies.gundabadWolfRider
      run $ CheckWindows [mkWhen $ EnemyAttacking rider pid]
      progressOn Quests.theEasternSpur `shouldReturn` 2

    it "makes the defending player declare his most damaged character against Ravenous Warg" . bofaTest $ \pid -> do
      warg <- engaged pid Enemies.ravenousWarg
      b <- beravor
      run $ DealDamage GameSource b 1
      _ <- setEncounterDeck [Locations.northernSlopes]
      run $ DealShadowCard warg
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ EnemyAttacks pid
      passActions pid
      passActions pid
      -- Beravor, the only damaged one, defends: 3 attack against 2 defense
      getDamage b `shouldReturn` 2

  describe "the locations" do
    it "gives Goblins +1 attack while Northern Slopes is in the staging area" . bofaTest $ \_ -> do
      rider <- stage Enemies.gundabadWolfRider
      getEnemyAttack rider `shouldReturn` 3
      run . AddToStaging =<< genEncounterCard Locations.northernSlopes
      getEnemyAttack rider `shouldReturn` 4

    it "costs 1 Baggins resource to travel to Ruins of Dale while stage 3 is current, 3 otherwise, and drains stage 3" . bofaTest $ \pid -> do
      setUp pid
      chooseStage pid Quests.theEasternSpur
      run . AddToStaging =<< genEncounterCard Locations.ruinsOfDale
      ruins <- locationOf Locations.ruinsOfDale
      b <- getHeroWithCode "132001"
      run $ PlaceTokens GameSource (toTarget b) Resource 1
      getModifiers (LocationTarget ruins) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      progressStage pid Quests.battleInTheDale 3
      getModifiers (LocationTarget ruins) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ CheckWindows [mkAfter RoundEnds]
      -- the Forced effects of the round's end, in the order offered
      answerAll pid
      progressOn Quests.battleInTheDale `shouldReturn` 1

  describe "the treacheries" do
    it "has Heedless of Order change the current quest for the round" . bofaTest $ \pid -> do
      toBattle
      chooseStage pid Quests.theEasternSpur
      reveal [Treacheries.heedlessOfOrder, Locations.northernSlopes]
      chooseTarget pid =<< stageOf Quests.battleInTheDale
      (getCurrentQuest `shouldReturn`) . Just =<< stageOf Quests.battleInTheDale

    it "raises each player's threat by 3 when Heedless of Order finds no other quest" . bofaTest $ \pid -> do
      _ <- putQuestStage Quests.theCloudsBurst
      threat <- (.threat) <$> getPlayer pid
      reveal [Treacheries.heedlessOfOrder, Locations.northernSlopes]
      (.threat) <$> getPlayer pid `shouldReturn` (threat + 3)

    it "has Hatred Rekindled surge when no enemy is engaged" . bofaTest $ \_ -> do
      reveal [Treacheries.hatredRekindledTheBattleOfFiveArmies, Locations.northernSlopes]
      _ <- locationOf Locations.northernSlopes
      pure ()

    it "has Goblins are Upon You! engage a Goblin from the encounter deck with each player" . bofaTest $ \pid -> do
      reveal [Treacheries.goblinsAreUponYou, Enemies.gundabadArcher]
      chooseOptionMatching "take the Goblin" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      archer <- enemyOf Enemies.gundabadArcher
      getsGame (fmap (.placement) . find ((== archer) . toId) . toList . (.entities.enemies)) `shouldReturn` Just (EngagedWith pid)

  describe "Misty Mountain Eagle" do
    it "lets the first player spend 2 Baggins resources to take control of it" . bofaTest $ \pid -> do
      setUp pid
      chooseStage pid Quests.theEasternSpur
      b <- getHeroWithCode "132001"
      run $ PlaceTokens GameSource (toTarget b) Resource 2
      reveal [Objectives.mistyMountainEagle]
      eagle <- getsGame (toTarget . fromJustNote "no eagle" . find ((== toCardCode Objectives.mistyMountainEagle) . toCardCode) . toList . (.entities.objectives))
      run $ ActionWindow pid
      chooseAbilityOf pid eagle
      chooseLabel "$label.mistyMountainEagle.takeControl" pid
      getsGame (any ((== toCardCode Objectives.mistyMountainEagle) . toCardCode) . toList . (.entities.allies)) `shouldReturn` True
