module LordOfTheRings.FlameOfTheWest.TheBattleOfThePelennorFieldsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Ally.Types (Ally)
import LordOfTheRings.Card (CardCost (StaticCost), CardDef (cdCost, cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Classes.Query (select)
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (AllyMatcher (AllyControlledBy), LocationMatcher (IsActiveLocation), PlayerMatcher (AnyPlayer), getAlly, getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theBattleOfThePelennorFields)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

pelennorTest :: (PlayerId -> GameT ()) -> IO ()
pelennorTest = gameTestIn theBattleOfThePelennorFields testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

setAsideCodes :: GameT [CardCode]
setAsideCodes = map toCardCode . scenarioSetAside <$> scenarioAttrs

-- | Setup, its stage 1A clicked, neither Esquire attached.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  chooseLabel "$label.pass" pid
  chooseLabel "$label.pass" pid

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

-- | Minas Tirith in the staging area, as stage 3A adds it.
minasTirith :: GameT ObjectiveId
minasTirith = do
  run . AddToStaging =<< genEncounterCard Objectives.minasTirith
  getsGame (fromJustNote "no Minas Tirith" . fmap toId . find ((== toCardCode Objectives.minasTirith) . toCardCode) . toList . (.entities.objectives))

damageOn :: ObjectiveId -> GameT Int
damageOn = getDamage . ObjectiveTarget

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

stagedAllies :: GameT [Ally]
stagedAllies = getsGame (filter ((== StagingArea) . (.placement)) . toList . (.entities.allies))

spec :: Spec
spec = describe "The Battle of the Pelennor Fields" do
  it "is a scenario the game can be dealt" . pelennorTest $ \_ ->
    isJust (lookupScenario "the-battle-of-the-pelennor-fields") `shouldBe` True

  describe "setup" do
    it "sets Minas Tirith, Grond, The Witch-king and 4 Wraiths on Wings aside, makes a Pelennor Field active and skips the quest and combat phases" . pelennorTest $ \pid -> do
      setUp pid
      sort <$> setAsideCodes `shouldReturn` sort (map toCardCode ([Objectives.minasTirith, Locations.grond, Enemies.theWitchKingCaptain] <> replicate 4 Enemies.wraithOnWings))
      actives <- select IsActiveLocation
      map toCardCode <$> traverse getLocation actives `shouldReturn` [toCardCode Locations.pelennorField]
      attrs <- scenarioAttrs
      all ((== Just TheBattleOfThePelennorFields) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      copies Locations.pelennorField attrs.encounterDeck `shouldBe` 3
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.retreatFromOsgiliath, Quests.theWhiteCityBesieged, Quests.rohanHasCome, Quests.fightingInTheFields]
      getModifiers GameTarget >>= (`shouldSatisfy` \ms -> SkipQuestPhase `elem` ms && SkipCombatPhase `elem` ms)
      (.owner) <$> getHeroWithCode (toCardCode Heroes.aragornTheFlameOfTheWest) `shouldReturn` pid

    it "lets the players attach the Esquires to different heroes" . pelennorTest $ \pid -> do
      flipFirstQuest pid
      beravor <- getHeroWithCode "01012"
      chooseTarget pid beravor.id
      -- Beravor has one, so the other goes elsewhere
      Just q <- getQuestion pid
      any (\case TargetLabel t _ -> t == toTarget beravor.id; _ -> False) (questionChoices q) `shouldBe` False
      eowyn <- getHeroWithCode "01007"
      chooseTarget pid eowyn.id
      attached <- getsGame (map (\a -> (toCardCode a, a.placement)) . toList . (.entities.attachments))
      attached `shouldSatisfy` elem (toCardCode Attachments.esquireOfRohan, AttachedTo (toTarget beravor.id))
      attached `shouldSatisfy` elem (toCardCode Attachments.esquireOfGondor, AttachedTo (toTarget eowyn.id))

  describe "The Garrison of Gondor" do
    it "places 1 progress at the end of each planning phase, advancing after the second" . pelennorTest $ \pid -> do
      setUp pid
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      activeQuestCode `shouldReturn` toCardCode Quests.theGarrisonOfGondor
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      activeQuestCode `shouldReturn` toCardCode Quests.retreatFromOsgiliath

  describe "Retreat from Osgiliath" do
    it "adds a Wraith on Wings, reveals a card and puts an ally from the deck into the staging area under no one's control" . pelennorTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.cityWall]
      run . CompleteQuest . toId =<< getActiveQuest
      resolveWhenRevealed pid
      resolveRevealed
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.wraithOnWings))
      allies <- stagedAllies
      length allies `shouldBe` 1
      for_ allies \a -> do
        getModifiers (AllyTarget a.id) >>= (`shouldSatisfy` \ms -> Blank `elem` ms && ImmuneToPlayerCardEffects `elem` ms)
        select (AllyControlledBy AnyPlayer) >>= (`shouldSatisfy` notElem a.id)

    it "has the allies' total cost as its quest points, and deals damage among them for each enemy in the staging area at the end of the round" . pelennorTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.cityWall]
      run . CompleteQuest . toId =<< getActiveQuest
      resolveWhenRevealed pid
      resolveRevealed
      [ally] <- stagedAllies
      let cost = case cdCost (toCardDef ally) of Just (StaticCost n) -> n; _ -> 0
      q <- toTarget <$> getActiveQuest
      when (cost > 1) do
        run $ PlaceTokens GameSource q Progress (cost - 1)
        activeQuestCode `shouldReturn` toCardCode Quests.retreatFromOsgiliath
      run $ CheckWindows [mkWhen RoundEnds]
      -- the Wraith on Wings in the staging area: 1 damage, assigned to the only ally
      chooseOptionMatching "assign the damage" pid (const True)
      getCharacterDamage (AllyTarget ally.id) >>= (`shouldSatisfy` (<= 1))

  describe "The White City Besieged" do
    it "hands the staging area's allies back exhausted and adds Minas Tirith, Grond and The Witch-king" . pelennorTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.cityWall]
      run . CompleteQuest . toId =<< getActiveQuest
      resolveWhenRevealed pid
      resolveRevealed
      [ally] <- stagedAllies
      run . CompleteQuest . toId =<< getActiveQuest
      resolveWhenRevealed pid
      a <- getAlly ally.id
      a.placement `shouldBe` InPlayArea pid
      allyExhausted (toAttrs a) `shouldBe` True
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.theWitchKingCaptain))
      getsGame (map toCardCode . toList . (.entities.objectives)) >>= (`shouldSatisfy` elem (toCardCode Objectives.minasTirith))
      getsGame (map toCardCode . toList . (.entities.locations)) >>= (`shouldSatisfy` elem (toCardCode Locations.grond))

    it "deals the quest failure to Minas Tirith instead of raising threat, and takes progress from destroyed enemies only" . pelennorTest $ \pid -> do
      q <- putQuestStage Quests.theWhiteCityBesieged
      run $ PlaceTokens GameSource q Progress 0
      g <- getGame
      putGame =<< preloadModifiers (g & entitiesL . questsL %~ fmap (overAttrs (\a -> a {questFlipped = True})))
      mt <- minasTirith
      eid <- stage Enemies.mordorOrc
      threat <- threatOf pid
      run QuestResolution
      threatOf pid `shouldReturn` threat
      damageOn mt `shouldReturn` 1
      run $ PlaceTokens GameSource q Progress 3
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0
      run $ DealDamage GameSource (EnemyTarget eid) 2
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 1

  describe "Minas Tirith" do
    it "takes 1 damage per enemy in the staging area at the end of the round, and is lost with none of its hit points left" . pelennorTest $ \_ -> do
      mt <- minasTirith
      _ <- stage Enemies.mordorOrc
      _ <- stage Enemies.mordorOrc
      run $ CheckWindows [mkWhen RoundEnds]
      damageOn mt `shouldReturn` 2
      run $ PlaceTokens GameSource (ObjectiveTarget mt) Damage 37
      run $ CheckWindows [mkWhen RoundEnds]
      getsGame (.outcome) `shouldReturn` Just Lost

    it "has an active City Wall take its damage first, the rest going on" . pelennorTest $ \pid -> do
      mt <- minasTirith
      wall <- stageLocation Locations.cityWall
      run $ PlaceLocation wall ActiveLocation
      threat <- threatOf pid
      run $ DealDamage GameSource (ObjectiveTarget mt) 6
      damageOn mt `shouldReturn` 2
      getsGame (Map.member wall . (.entities.locations)) `shouldReturn` False
      threatOf pid `shouldReturn` (threat + 3)

  describe "encounter cards" do
    it "War Mûmak reduces each player's threat by 4 as it is defeated" . pelennorTest $ \pid -> do
      eid <- stage Enemies.warMumak
      run $ RaiseThreat pid 10
      threat <- threatOf pid
      run $ DealDamage GameSource (EnemyTarget eid) 3
      run $ DealDamage GameSource (EnemyTarget eid) 3
      -- no more than 3 damage each round
      getDamage (EnemyTarget eid) `shouldReturn` 3
      run $ Destroy GameSource (EnemyTarget eid)
      threatOf pid `shouldReturn` (threat - 4)

    it "Spells of Ruin damages each exhausted character while Minas Tirith is not in play" . pelennorTest $ \_ -> do
      beravor <- getHeroWithCode "01012"
      run $ Exhaust (toTarget beravor.id)
      _ <- setEncounterDeck [Treacheries.spellsOfRuin]
      run RevealEncounterCard
      resolveRevealed
      getCharacterDamage (toTarget beravor.id) `shouldReturn` 1

    it "Wraith on Wings gains 1 resource at the beginning of each quest phase, +1 threat each" . pelennorTest $ \_ -> do
      eid <- stage Enemies.wraithOnWings
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      (.resources) <$> getEnemy eid `shouldReturn` 1
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem (ThreatModifier 1))

  describe "campaign mode" do
    let logged = lotr {campaignLogCounts = Map.fromList [(CorsairFleetResources, Map.singleton (toCardCode Objectives.theCorsairFleet) 1), (HaradEnemies, Map.singleton (toCardCode Enemies.warMumak) 2)]}
    it "sets the Harad enemies and Aragorn aside, and The Corsair Fleet brings him back with Andúril and Banner of Elendil" . campaignTestIn theBattleOfThePelennorFields logged testDeck $ \pid -> do
      setUp pid
      -- set aside by the campaign card, Aragorn has not left play: the game goes on
      getsGame (.outcome) `shouldReturn` Nothing
      length . filter (== toCardCode Enemies.warMumak) <$> setAsideCodes `shouldReturn` 2
      getsGame (any ((== toCardCode Heroes.aragornTheFlameOfTheWest) . toCardCode) . (.entities.heroes)) `shouldReturn` False
      fleet <- getsGame (fromJustNote "no fleet" . find ((== toCardCode Objectives.theCorsairFleetGondor) . toCardCode) . toList . (.entities.objectives))
      fleet.resources `shouldBe` 1
      -- stage 1B skips the quest phase: the fleet waits
      g <- getGame
      putGame =<< preloadModifiers (g & entitiesL . questsL %~ fmap (overAttrs (\a -> a {questFlipped = False})))
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      aragorn <- getHeroWithCode (toCardCode Heroes.aragornTheFlameOfTheWest)
      aragorn.owner `shouldBe` pid
      attached <- getsGame (map toCardCode . filter ((== AttachedTo (toTarget aragorn.id)) . (.placement)) . toList . (.entities.attachments))
      sort attached `shouldBe` sort (map toCardCode [Attachments.andurilTheRoadDarkens, Attachments.bannerOfElendil])
      map toCardCode . (.victoryDisplay) <$> scenarioAttrs >>= (`shouldSatisfy` elem (toCardCode Objectives.theCorsairFleetGondor))

    it "earns a Wraith on Wings for each player, and the Esquires for their heroes' controllers" . campaignTestIn theBattleOfThePelennorFields lotr testDeck $ \pid -> do
      flipFirstQuest pid
      beravor <- getHeroWithCode "01012"
      chooseTarget pid beravor.id
      chooseLabel "$label.pass" pid
      run $ GameOver Won
      l <- getCampaignLog
      l.pool `shouldSatisfy` elem (PoolCard (toCardCode Enemies.wraithOnWings) Nothing Nothing)
      l.pool `shouldSatisfy` elem (PoolCard (toCardCode Attachments.esquireOfRohan) (Just 0) (Just "01012"))
      length l.pool `shouldBe` 2

    it "adds the set aside Harad enemies as the players advance to stage 5B" . campaignTestIn theBattleOfThePelennorFields lotr testDeck $ \pid -> do
      card <- genEncounterCard Enemies.warMumak
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [card])
      q <- putQuestStage Quests.fightingInTheFields
      run $ Flip q
      resolveWhenRevealed pid
      run $ CheckWindows [mkAfter AdvancedToStage]
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.warMumak))
