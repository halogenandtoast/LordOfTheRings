module LordOfTheRings.AngmarAwakened.AcrossTheEttenmoorsSpec (spec) where

import LordOfTheRings.Card (CardDef (..), toCard, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Safe))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher.Types (CardMatcher (AnyCard))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Types (isSideQuest)
import LordOfTheRings.Scenario (acrossTheEttenmoors, lookupScenario)
import LordOfTheRings.Scenario.Types (scenarioVictoryDisplay)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PhaseBegins, PhaseEnds), mkWhen)
import TestImport

ettenTest :: (PlayerId -> GameT ()) -> IO ()
ettenTest = gameTestIn acrossTheEttenmoors testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [a.placement | a <- toList g.entities.allies, toCardCode a == code]
    <> [t.placement | t <- toList g.entities.treacheries, toCardCode t == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

questOf :: CardDef -> GameT Quest
questOf def = getsGame (fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The location's guard, the card id of the encounter card it is attached to.
guardedBy :: CardDef -> CardDef -> GameT ()
guardedBy location guarding = do
  g <- getGame
  let cid = headMay [(toCard x).id | x <- toList g.entities.enemies, toCardCode x == toCardCode guarding]
        <|> headMay [(toCard x).id | x <- toList g.entities.locations, toCardCode x == toCardCode guarding]
        <|> headMay [(toCard x).id | x <- toList g.entities.quests, toCardCode x == toCardCode guarding]
  placementOf location `shouldReturn` (AttachedTo . CardIdTarget <$> cid)

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

beravor, eowyn :: GameT Target
beravor = hero "01012"
eowyn = hero "01007"

spec :: Spec
spec = describe "Across the Ettenmoors" do
  it "is a scenario the game can be dealt" . ettenTest $ \_ ->
    isJust (lookupScenario "across-the-ettenmoors") `shouldBe` True

  describe "Setup" do
    it "stages a side quest guarding a safe location, a Savage Trollspawn per player, and gives Amarthiúl to the first player" . ettenTest $ \pid -> do
      flipFirstQuest pid
      sideQuests <- getsGame (filter isSideQuest . toList . (.entities.quests))
      length sideQuests `shouldBe` 1
      safe <- getsGame (filter (member Safe . cdKeywords . toCardDef) . toList . (.entities.locations))
      map (.placement) safe `shouldBe` [AttachedTo (CardIdTarget (toCard q).id) | q <- sideQuests]
      trollspawn <- getsGame (filter ((== toCardCode Enemies.savageTrollspawn) . toCardCode) . toList . (.entities.enemies))
      map (.placement) trollspawn `shouldBe` [StagingArea]
      placementOf Objectives.amarthiulAcrossTheEttenmoors `shouldReturn` Just (InPlayArea pid)

  describe "Guarded locations" do
    it "are guarded by the next encounter card, and go to the staging area once the side quest guarding is defeated" . ettenTest $ \_ -> do
      _ <- putQuestStage Quests.intoTheEttenmoors
      reveal [Locations.secludedCave, Quests.forageForFood]
      Locations.secludedCave `guardedBy` Quests.forageForFood
      forage <- questOf Quests.forageForFood
      run $ CompleteQuest (toId forage)
      placementOf Locations.secludedCave `shouldReturn` Just StagingArea
      victoryDisplay `shouldReturn` [toCardCode Quests.forageForFood]
      -- Into the Ettenmoors takes the side quest's quest points in progress
      (.progress) <$> questOf Quests.intoTheEttenmoors `shouldReturn` 6

    it "go free as the enemy guarding them is discarded" . ettenTest $ \_ -> do
      reveal [Locations.theHoarwell, Enemies.goblinPursuer]
      Locations.theHoarwell `guardedBy` Enemies.goblinPursuer
      run . Discard . toTarget =<< enemyOf Enemies.goblinPursuer
      placementOf Locations.theHoarwell `shouldReturn` Just StagingArea

    it "can be shuffled into the encounter deck by Ruthless Hill-troll's shadow, the guard staying" . ettenTest $ \pid -> do
      reveal [Locations.abandonedCamp, Enemies.goblinPursuer]
      troll <- staged Enemies.ruthlessHillTroll
      _ <- setEncounterDeck [Enemies.ruthlessHillTroll]
      run $ EngageEnemy pid troll
      run $ DealShadowCard troll
      shadow <- getsGame (fromJustNote "no shadow" . headMay . concatMap (.shadowCards) . toList . (.entities.enemies))
      run $ ResolveShadowCard shadow.id troll pid []
      placementOf Locations.abandonedCamp `shouldReturn` Nothing
      placementOf Enemies.goblinPursuer `shouldReturn` Just StagingArea

  describe "Troll-fells" do
    it "searches out a safe location to guard as it is revealed" . ettenTest $ \pid -> do
      _ <- setEncounterDeck [Locations.trollFells]
      hoarwell <- genEncounterCard Locations.theHoarwell
      run $ AddToEncounterDiscard [hoarwell]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid hoarwell
      Locations.theHoarwell `guardedBy` Locations.trollFells
      -- explored, it sets the safe location free
      fells <- locationOf Locations.trollFells
      run $ PlaceTokens GameSource (toTarget fells) Progress 6
      placementOf Locations.theHoarwell `shouldReturn` Just StagingArea

    it "searches for nothing when it is revealed guarding a safe location" . ettenTest $ \_ -> do
      reveal [Locations.patchOfWoods, Locations.trollFells, Locations.theHoarwell]
      Locations.patchOfWoods `guardedBy` Locations.trollFells
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.theHoarwell]

  describe "Safe locations" do
    it "return each engaged enemy to the staging area as the players travel there" . ettenTest $ \pid -> do
      spawn <- staged Enemies.savageTrollspawn
      run $ EngageEnemy pid spawn
      patch <- stagedLocation Locations.patchOfWoods
      run $ TravelTo patch
      placementOf Enemies.savageTrollspawn `shouldReturn` Just StagingArea

    it "stop engagement checks, blank encounter side quests, and go to the victory display at the end of the quest phase" . ettenTest $ \_ -> do
      run . AddToStaging =<< genEncounterCard Quests.scavengeForSupplies
      getModifiers GameTarget >>= (`shouldSatisfy` elem (IncreaseCost AnyCard 1))
      patch <- stagedLocation Locations.patchOfWoods
      run $ PlaceLocation patch ActiveLocation
      getModifiers GameTarget >>= (`shouldSatisfy` \ms -> NoEngagementChecks `elem` ms && IncreaseCost AnyCard 1 `notElem` ms)
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      victoryDisplay `shouldReturn` [toCardCode Locations.patchOfWoods]

    it "ignore the when revealed effects of treacheries while active" . ettenTest $ \_ -> do
      run . Exhaust =<< eowyn
      patch <- stagedLocation Locations.patchOfWoods
      run $ PlaceLocation patch ActiveLocation
      reveal [Treacheries.noRest]
      placementOf Treacheries.noRest `shouldReturn` Nothing
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Treacheries.noRest]

  describe "The quest" do
    it "discards down to a side quest as the quest phase begins with none in play" . ettenTest $ \_ -> do
      _ <- putQuestStage Quests.intoTheEttenmoors
      _ <- setEncounterDeck [Enemies.savageTrollspawn, Quests.lieLow, Enemies.goblinPursuer]
      run $ CheckWindows [Window After (PhaseBegins QuestPhase)]
      isSideQuest <$> questOf Quests.lieLow `shouldReturn` True
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.savageTrollspawn]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.goblinPursuer]

    it "reveals a side quest, and an additional card each staging step, on A Miserable Journey" . ettenTest $ \_ -> do
      q <- putQuestStage Quests.aMiserableJourney
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))
      _ <- setEncounterDeck [Enemies.goblinPursuer, Quests.forageForFood]
      run $ WhenRevealed q
      isSideQuest <$> questOf Quests.forageForFood `shouldReturn` True
      patch <- stagedLocation Locations.patchOfWoods
      run $ PlaceLocation patch ActiveLocation
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))

    it "gives each damaged character -2 willpower on Journey's End" . ettenTest $ \_ -> do
      _ <- putQuestStage Quests.journeysEnd
      b <- beravor
      run $ DealDamage GameSource b 1
      getModifiers b >>= (`shouldSatisfy` elem (WillpowerModifier (-2)))

  describe "Side quests" do
    it "Lie Low lowers each enemy's engagement cost by 20 when no progress was placed on it" . ettenTest $ \_ -> do
      run . AddToStaging =<< genEncounterCard Quests.lieLow
      spawn <- staged Enemies.savageTrollspawn
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      getModifiers (EnemyTarget spawn) >>= (`shouldSatisfy` elem (EngagementCostModifier (-20)))

    it "Forage for Food deals 1 damage to a hero resources are spent from" . ettenTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Quests.forageForFood
      b <- beravor
      run $ PlaceTokens GameSource b Resource 2
      run $ RemoveTokens (PlayerSource pid) b Resource 2
      getDamage b `shouldReturn` 1

  describe "Encounter cards" do
    it "Savage Trollspawn gets +1 attack for each damaged character the engaged player controls" . ettenTest $ \pid -> do
      spawn <- staged Enemies.savageTrollspawn
      run $ EngageEnemy pid spawn
      run . (\t -> DealDamage GameSource t 1) =<< beravor
      getModifiers (EnemyTarget spawn) >>= (`shouldSatisfy` elem (AttackModifier 1))

    it "Coldfell Giant deals 1 damage to 3 different characters as it engages" . ettenTest $ \pid -> do
      giant <- staged Enemies.coldfellGiant
      run $ EngageEnemy pid giant
      heroes <- getsGame (map toTarget . toList . (.entities.heroes))
      traverse getDamage heroes `shouldReturn` [1, 1, 1]

    it "Cruel Mountain-troll's excess damage removes progress from each quest card" . ettenTest $ \pid -> do
      main <- putQuestStage Quests.intoTheEttenmoors
      run $ PlaceTokens GameSource main Progress 3
      troll <- staged Enemies.cruelMountainTroll
      b <- beravor
      -- Beravor has 4 hit points: 2 of the 6 damage are excess
      msgs <- attackDamage pid troll b 6
      msgs `shouldSatisfy` elem (RemoveTokens (EnemyAttackSource troll) main Progress 2)

    it "Ruthless Hill-troll gets +2 attack and +2 defense with 3 damage on it" . ettenTest $ \_ -> do
      troll <- staged Enemies.ruthlessHillTroll
      run $ DealDamage GameSource (EnemyTarget troll) 3
      getModifiers (EnemyTarget troll) >>= (`shouldSatisfy` \ms -> AttackModifier 2 `elem` ms && DefenseModifier 2 `elem` ms)

    it "Goblin Pursuer cannot be fought by damaged characters" . ettenTest $ \_ -> do
      goblin <- staged Enemies.goblinPursuer
      b <- beravor
      canFight goblin b `shouldReturn` True
      run $ DealDamage GameSource b 1
      canFight goblin b `shouldReturn` False

    it "Barren Moorland deals 1 damage to each exhausted character as the players travel there" . ettenTest $ \_ -> do
      e <- eowyn
      run $ Exhaust e
      moor <- stagedLocation Locations.barrenMoorland
      run $ TravelTo moor
      getDamage e `shouldReturn` 1

    it "No Rest attaches to an exhausted hero, and is discarded as the players travel to a safe location" . ettenTest $ \_ -> do
      e <- eowyn
      run $ Exhaust e
      reveal [Treacheries.noRest]
      placementOf Treacheries.noRest `shouldReturn` Just (AttachedTo e)
      getModifiers e >>= (`shouldSatisfy` \ms -> CannotReady `elem` ms && CannotCollectResources `elem` ms)
      patch <- stagedLocation Locations.patchOfWoods
      run $ TravelTo patch
      placementOf Treacheries.noRest `shouldReturn` Nothing

    it "Arador's Bane has a player engage an enemy he chooses, dealt 2 shadow cards" . ettenTest $ \pid -> do
      spawn <- staged Enemies.savageTrollspawn
      _ <- setEncounterDeck [Treacheries.aradorsBane, Enemies.goblinPursuer, Enemies.goblinPursuer]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid spawn
      placementOf Enemies.savageTrollspawn `shouldReturn` Just (EngagedWith pid)
      shadows <- getsGame (concatMap (.shadowCards) . toList . (.entities.enemies))
      length shadows `shouldBe` 2

    it "loses the game as Amarthiúl leaves play" . ettenTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.amarthiulAcrossTheEttenmoors
      placementOf Objectives.amarthiulAcrossTheEttenmoors `shouldReturn` Just (InPlayArea pid)
      amarthiul <- getsGame (fromJustNote "no Amarthiúl" . fmap toTarget . headMay . toList . (.entities.allies))
      run $ Discard amarthiul
      getsGame (.outcome) `shouldReturn` Just Lost
