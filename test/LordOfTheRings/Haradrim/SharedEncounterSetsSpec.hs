module LordOfTheRings.Haradrim.SharedEncounterSetsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower, getAttack)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getEngagementCost, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithId), getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

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

placementOfEnemy :: EnemyId -> GameT (Maybe Placement)
placementOfEnemy eid = getsGame (fmap (.placement) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

questOf :: CardDef -> GameT Target
questOf def = getsGame (toTarget . fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questsInPlay :: GameT [CardCode]
questsInPlay = getsGame (map toCardCode . toList . (.entities.quests))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

-- | The main quest in play, with the given progress on it.
mainQuest :: Int -> GameT Target
mainQuest n = do
  q <- putQuestStage Quests.fliesAndSpiders
  when (n > 0) $ run $ PlaceTokens GameSource q Progress n
  pure q

-- | The enemy attacks the defender with Giant Scorpion dealt to it as its shadow card.
scorpionShadowAttack :: PlayerId -> EnemyId -> Target -> GameT ()
scorpionShadowAttack pid eid defender = do
  _ <- setEncounterDeck [Enemies.giantScorpion]
  run $ DealShadowCard eid
  putGame . (phaseL .~ CombatPhase) =<< getGame
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

spec :: Spec
spec = describe "The Sands of Harad's shared encounter sets" do
  describe "Harad Soldiers" do
    it "makes the first Harad enemy discarded Southron Champion's Champion, guarding both stages until it is destroyed" . gameTest $ \_ -> do
      main <- mainQuest 0
      reveal [Quests.southronChampion, Enemies.forestSpider, Enemies.southronSoldier]
      champ <- questOf Quests.southronChampion
      soldier <- enemyOf Enemies.southronSoldier
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.forestSpider]
      getEnemyThreat soldier `shouldReturn` 5
      getEnemyAttack soldier `shouldReturn` 5
      getModifiers soldier >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getModifiers main >>= (`shouldSatisfy` elem CannotBeDefeated)
      -- no damage until it has 4 progress, which cannot defeat it while the Champion lives
      run $ DealDamage GameSource (toTarget soldier) 2
      getDamage (toTarget soldier) `shouldReturn` 0
      run $ PlaceTokens GameSource champ Progress 4
      questsInPlay >>= (`shouldSatisfy` elem (toCardCode Quests.southronChampion))
      run $ DealDamage GameSource (toTarget soldier) 5
      questsInPlay `shouldReturn` [toCardCode Quests.fliesAndSpiders]
      victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Quests.southronChampion))
      getModifiers main >>= (`shouldSatisfy` notElem CannotBeDefeated)

    it "takes for the Champion only the enemy Southron Champion marked, not another Harad enemy with a resource" . gameTest $ \_ -> do
      _ <- mainQuest 0
      archer <- stage Enemies.southronArcher
      run $ PlaceTokens GameSource (toTarget archer) Resource 1
      threat <- getEnemyThreat archer
      reveal [Quests.southronChampion, Enemies.southronSoldier]
      getEnemyThreat archer `shouldReturn` threat
      getModifiers archer >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)

    it "gives other Harad enemies +1 threat while Southron Captain is in the staging area" . gameTest $ \_ -> do
      archer <- stage Enemies.southronArcher
      _ <- stage Enemies.southronCaptain
      getEnemyThreat archer `shouldReturn` 3

    it "has Southron Soldier remove 2 progress from the main quest or attack as it engages" . gameTest $ \pid -> do
      _ <- mainQuest 2
      _ <- engaged pid Enemies.southronSoldier
      chooseLabelStarting "$label.removeMainQuestProgress" pid
      getsGame (sum . map (.progress) . toList . (.entities.quests)) `shouldReturn` 0
      -- with no progress left to remove, it attacks
      _ <- engaged pid Enemies.southronSoldier
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "has Southron Soldier's shadow engage an enemy in the staging area, dealt a shadow card, to attack this step" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      troll <- stage Enemies.hillTroll
      _ <- setEncounterDeck [Enemies.southronSoldier, Locations.oldForestRoad]
      run $ DealShadowCard spider
      putGame . (phaseL .~ CombatPhase) =<< getGame
      push $ PlayerAttacks pid
      run $ EnemyAttacks pid
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      chooseTarget pid =<< hero "01012"
      passActions pid
      placementOfEnemy troll `shouldReturn` Just (EngagedWith pid)
      fmap (.shadowCards) (getEnemy troll) >>= (`shouldSatisfy` notNull)
      -- the Hill Troll attacks next, its prompt about it
      Just (QuestionLabel _ source _ _) <- getQuestion pid
      source `shouldBe` Just (toTarget troll)

    it "has Southron Archer discard an ally when there is no progress to remove" . gameTest $ \pid -> do
      _ <- mainQuest 0
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- engaged pid Enemies.southronArcher
      getsGame (length . (.entities.allies)) `shouldReturn` 0

    it "has Nowhere to Hide engage the first enemy discarded with the first player" . gameTest $ \pid -> do
      reveal [Treacheries.nowhereToHide, Locations.oldForestRoad, Enemies.southronSoldier]
      soldier <- enemyOf Enemies.southronSoldier
      placementOfEnemy soldier `shouldReturn` Just (EngagedWith pid)
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.oldForestRoad))

  describe "Harad Territory" do
    it "has each player draw a card as In Enemy Territory is defeated" . gameTest $ \_ -> do
      _ <- mainQuest 0
      reveal [Quests.inEnemyTerritory, Locations.oldForestRoad]
      lead <- getLead
      drawn <- handSize lead
      q <- questOf Quests.inEnemyTerritory
      run $ PlaceTokens GameSource q Progress 8
      handSize lead `shouldReturn` (drawn + 1)

    it "raises a player's threat by 1 for each character he controls when he chooses so for Injured Companion" . gameTest $ \pid -> do
      reveal [Treacheries.injuredCompanion]
      chooseLabelStarting "$label.injuredCompanion.raiseThreat" pid
      (.threat) <$> getPlayer pid `shouldReturn` 3

    it "bars every character from readying until the end of the combat phase after Enfeebled" . gameTest $ \pid -> do
      reveal [Treacheries.enfeebled]
      eowyn <- hero "01007"
      chooseTarget pid eowyn
      exhausted "01007" `shouldReturn` True
      run $ Ready eowyn
      exhausted "01007" `shouldReturn` True
      runOnly $ EndPhase QuestPhase
      getModifiers GameTarget >>= (`shouldSatisfy` elem CannotReady)
      runOnly $ EndPhase CombatPhase
      getModifiers GameTarget >>= (`shouldSatisfy` notElem CannotReady)

  describe "Desert Sands" do
    it "readies an exhausted hero of each player as Find Water is defeated, though no player card may ready one" . gameTest $ \_ -> do
      _ <- mainQuest 0
      reveal [Quests.findWater]
      eowyn <- hero "01007"
      run $ Exhaust eowyn
      q <- questOf Quests.findWater
      run $ PlaceTokens GameSource q Progress 6
      exhausted "01007" `shouldReturn` False

    it "lets no progress be placed on other locations in the staging area while Towering Dunes is there" . gameTest $ \_ -> do
      _ <- stageLocation Locations.toweringDunes
      road <- stageLocation Locations.oldForestRoad
      hid <- getsGame (fromJustNote "no hero" . headMay . map toId . toList . (.entities.heroes))
      run $ PlaceTokens (HeroSource hid) (toTarget road) Progress 2
      (.progress) <$> getLocation road `shouldReturn` 0

    it "has Towering Dunes reveal an encounter card as it becomes the active location" . gameTest $ \_ -> do
      dunes <- stageLocation Locations.toweringDunes
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ TravelTo dunes
      getsGame (length . (.entities.locations)) `shouldReturn` 2

    it "heals 4 damage from among characters as the active Desert Oasis is explored" . gameTest $ \pid -> do
      oasis <- stageLocation Locations.desertOasis
      run $ TravelTo oasis
      -- each player exhausts a character as it becomes active
      chooseTarget pid =<< hero "01012"
      exhausted "01012" `shouldReturn` True
      beravor <- hero "01012"
      run $ DealDamage GameSource beravor 3
      run $ PlaceTokens GameSource (toTarget oasis) Progress 2
      chooseAbilityOf pid oasis
      replicateM_ 3 $ getQuestion pid >>= \case
        Just _ -> chooseTarget pid beravor
        Nothing -> pure ()
      getDamage beravor `shouldReturn` 0

    it "has each player discard his hand for Sandstorm when he chooses so" . gameTest $ \pid -> do
      replicateM_ 2 $ putCardIntoHand pid Allies.guardOfTheCitadel
      reveal [Treacheries.sandstorm]
      chooseLabel "$label.sandstorm.discardHand" pid
      handSize pid `shouldReturn` 0

    it "has each player exhaust and damage a character for Heat Exhaustion" . gameTest $ \pid -> do
      reveal [Treacheries.heatExhaustion]
      chooseTarget pid =<< hero "01007"
      exhausted "01007" `shouldReturn` True
      (getDamage =<< hero "01007") `shouldReturn` 1

  describe "Desert Creatures" do
    it "blanks damaged characters while Craft Remedy is in play, and heals them as it is defeated" . gameTest $ \_ -> do
      _ <- mainQuest 0
      reveal [Quests.craftRemedy]
      beravor <- hero "01012"
      run $ DealDamage GameSource beravor 2
      getModifiers beravor >>= (`shouldSatisfy` elem Blank)
      q <- questOf Quests.craftRemedy
      run $ PlaceTokens GameSource q Progress 6
      getDamage beravor `shouldReturn` 1

    it "has Sand Viper engage the first player after the players travel, undefendable" . gameTest $ \pid -> do
      viper <- stage Enemies.sandViper
      getModifiers viper >>= (`shouldSatisfy` elem CannotBeDefended)
      road <- stageLocation Locations.oldForestRoad
      run $ TravelTo road
      placementOfEnemy viper `shouldReturn` Just (EngagedWith pid)

    it "bars a character Giant Scorpion damages from readying this round" . gameTest $ \_ -> do
      scorpion <- stage Enemies.giantScorpion
      eowyn <- hero "01007"
      run $ DealDamage (EnemyAttackSource scorpion) eowyn 1
      getModifiers eowyn >>= (`shouldSatisfy` elem CannotReady)

    it "bars the defender from readying this round if the attack Giant Scorpion's shadow is dealt to damages it" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      eowyn <- hero "01007"
      scorpionShadowAttack pid spider eowyn
      getCharacterDamage eowyn >>= (`shouldSatisfy` (> 0))
      getModifiers eowyn >>= (`shouldSatisfy` elem CannotReady)

    it "leaves the defender free to ready if the attack Giant Scorpion's shadow is dealt to deals it no damage" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      eowyn <- hero "01007"
      run $ CreateEffect $ Effect GameSource eowyn [CannotTakeDamage] UntilEndOfRound
      scorpionShadowAttack pid spider eowyn
      getModifiers eowyn >>= (`shouldSatisfy` notElem CannotReady)

  describe "Jungle Forest" do
    it "lowers the staging area's locations' threat while Jungle Trail is active, revealing a card to travel there" . gameTest $ \_ -> do
      trail <- stageLocation Locations.jungleTrail
      road <- stageLocation Locations.oldForestRoad
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ TravelTo trail
      getsGame (length . (.entities.locations)) `shouldReturn` 3
      getLocationThreat road `shouldReturn` 0

    it "adds the enemy Forest Clearing discards to the staging area as it is explored" . gameTest $ \_ -> do
      clearing <- stageLocation Locations.forestClearing
      run $ TravelTo clearing
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ PlaceTokens GameSource (toTarget clearing) Progress 2
      spider <- enemyOf Enemies.forestSpider
      placementOfEnemy spider `shouldReturn` Just StagingArea

    it "attaches Overgrown to the highest threat location without one, +1 threat and immune" . gameTest $ \_ -> do
      road <- stageLocation Locations.oldForestRoad
      trail <- stageLocation Locations.jungleTrail
      reveal [Treacheries.overgrown, Locations.desertOasis]
      getLocationThreat trail `shouldReturn` 5
      getLocationThreat road `shouldReturn` 1
      getModifiers trail >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      -- it surged
      getsGame (length . (.entities.locations)) `shouldReturn` 3

  describe "Mordor Orcs" do
    it "lowers staged Orcs' engagement cost and raises engaged ones' threat while Uruks of the Eye is in play" . gameTest $ \pid -> do
      _ <- mainQuest 0
      staged <- stage Enemies.urukOfMordor
      fought <- engaged pid Enemies.urukWargRider
      reveal [Quests.uruksOfTheEye, Locations.oldForestRoad]
      getEngagementCost staged `shouldReturn` 30
      getEnemyThreat fought `shouldReturn` 4

    it "keeps Uruk Chieftain from damage while its player is engaged with another Orc, and has it engage the player with the most Orcs" . twoPlayerTest testDeck $ \one two -> do
      chieftain <- engaged one Enemies.urukChieftain
      uruk <- engaged one Enemies.urukOfMordor
      run $ DealDamage GameSource (toTarget chieftain) 2
      getDamage (toTarget chieftain) `shouldReturn` 0
      run $ EngageEnemy two uruk
      run $ DealDamage GameSource (toTarget chieftain) 2
      getDamage (toTarget chieftain) `shouldReturn` 2
      run $ CheckWindows [mkWhen $ PhaseEnds EncounterPhase]
      placementOfEnemy chieftain `shouldReturn` Just (EngagedWith two)

    it "bars allies with less willpower than Uruk of Mordor's threat from defending against it" . gameTest $ \pid -> do
      uruk <- engaged pid Enemies.urukOfMordor
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      getModifiers guardian >>= (`shouldSatisfy` elem (CannotDefendAgainst (EnemyWithId uruk)))
      eowyn <- hero "01007"
      getModifiers eowyn >>= (`shouldSatisfy` notElem (CannotDefendAgainst (EnemyWithId uruk)))

    it "raises Uruk Warg-Rider's attack by the difference while it attacks a character of less willpower" . gameTest $ \pid -> do
      rider <- engaged pid Enemies.urukWargRider
      beravor <- hero "01012"
      w <- getWillpower beravor
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run DealShadowCards
      run $ DefendWith pid rider beravor
      getEnemyAttack rider `shouldReturn` (3 + max 0 (3 - w))

    it "attaches Mordor Warg to an Orc, returning it to the staging area, and adds the Warg there once the Orc leaves play" . gameTest $ \pid -> do
      uruk <- engaged pid Enemies.urukOfMordor
      reveal [Enemies.mordorWarg]
      warg <- enemyOf Enemies.mordorWarg
      placementOfEnemy warg `shouldReturn` Just (AttachedTo (EnemyTarget uruk))
      placementOfEnemy uruk `shouldReturn` Just StagingArea
      getEnemyThreat uruk `shouldReturn` 4
      run $ DealDamage GameSource (toTarget uruk) 6
      placementOfEnemy uruk `shouldReturn` Nothing
      placementOfEnemy warg `shouldReturn` Just StagingArea

    it "returns Mordor Warg to the staging area as the Uruk Chieftain it rides goes to the victory display" . gameTest $ \_ -> do
      chieftain <- stage Enemies.urukChieftain
      reveal [Enemies.mordorWarg]
      warg <- enemyOf Enemies.mordorWarg
      run $ DealDamage GameSource (toTarget chieftain) 8
      victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Enemies.urukChieftain))
      placementOfEnemy warg `shouldReturn` Just StagingArea

    it "gives each ally -1 willpower, attack and defense this round for Fear of Mordor" . gameTest $ \pid -> do
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      reveal [Treacheries.fearOfMordor]
      getWillpower guardian `shouldReturn` 0
      getAttack guardian `shouldReturn` 0
