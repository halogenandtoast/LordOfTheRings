module LordOfTheRings.AgainstTheShadow.EncounterAtAmonDinSpec (spec) where

import LordOfTheRings.Ally (Ally)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (ScenarioAttrs (..), encounterAtAmonDin, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (
  deadVillagerCount,
  getVillagers,
  objectiveOf,
  rescuedVillagerCount,
 )
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkAfter)
import TestImport

amonDinTest :: (PlayerId -> GameT ()) -> IO ()
amonDinTest = gameTestIn encounterAtAmonDin testDeck

-- | The setup, with an encounter deck of the cards it takes out and Secluded Farmhouse to
-- reveal.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseLabel "$label.keepHand" pid
  _ <-
    setEncounterDeck
      [ Enemies.ghulat
      , Objectives.lordAlcaron
      , Locations.burningFarmhouse
      , Objectives.rescuedVillagers
      , Objectives.deadVillagers
      , Locations.secludedFarmhouse
      ]
  chooseOptionMatching "flip the quest" pid (const True)

-- | Rescued Villagers and Dead Villagers in the staging area, with villagers rescued.
objectives :: Int -> GameT Target
objectives rescued = do
  for_ [Objectives.rescuedVillagers, Objectives.deadVillagers] \d -> run . AddToStaging =<< genEncounterCard d
  o <- objectiveTarget Objectives.rescuedVillagers
  when (rescued > 0) $ run $ PlaceTokens GameSource o Villager rescued
  pure o

objectiveTarget :: CardDef -> GameT Target
objectiveTarget def = ObjectiveTarget . fromJustNote "no such objective" <$> objectiveOf def

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT LocationId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  run . AddToStaging =<< genEncounterCard def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

alcaron :: GameT (Maybe Ally)
alcaron = getsGame (find ((== toCardCode Objectives.lordAlcaron) . toCardCode) . toList . (.entities.allies))

spec :: Spec
spec = describe "Encounter at Amon Dîn" do
  it "is a scenario the game can be dealt" . amonDinTest $ \_ ->
    isJust (lookupScenario "encounter-at-amon-din") `shouldBe` True

  it "sets aside Ghulat, gives the first player Lord Alcaron and makes Burning Farmhouse active at setup" . amonDinTest $ \pid -> do
    setUp pid
    farmhouse <- locationOf Locations.burningFarmhouse
    (.placement) <$> getLocation farmhouse `shouldReturn` ActiveLocation
    getVillagers (toTarget farmhouse) `shouldReturn` 4
    fmap (.placement) <$> alcaron `shouldReturn` Just (InPlayArea pid)
    (getVillagers . toTarget =<< getActiveQuest) `shouldReturn` 5
    (getVillagers . toTarget =<< locationOf Locations.secludedFarmhouse) `shouldReturn` 2
    for_ [Objectives.rescuedVillagers, Objectives.deadVillagers] \d -> isJust <$> objectiveOf d `shouldReturn` True
    getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.ghulat]

  it "moves villagers from Savagery of the Orcs to Rescued Villagers for progress, and advances once none are left" . amonDinTest $ \pid -> do
    setUp pid
    quest <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource quest Progress 3
    getVillagers quest `shouldReturn` 2
    rescuedVillagerCount `shouldReturn` 3
    run $ PlaceTokens GameSource quest Progress 4
    rescuedVillagerCount `shouldReturn` 5
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.protectTheVillagers
    resolveWhenRevealed pid
    (.placement) <$> (getEnemy =<< enemyOf Enemies.ghulat) `shouldReturn` StagingArea

  it "moves an explored location's villagers to Rescued Villagers" . amonDinTest $ \_ -> do
    _ <- objectives 0
    hamlet <- stage Locations.gondorianHamlet
    run $ PlaceTokens GameSource (toTarget hamlet) Progress 4
    rescuedVillagerCount `shouldReturn` 3
    getsGame ((.victoryPoints) . toAttrs . (.scenario)) `shouldReturn` 1

  it "discards a villager from Burning Farmhouse at the end of the round, killing it" . amonDinTest $ \_ -> do
    _ <- objectives 0
    farmhouse <- stage Locations.burningFarmhouse
    run $ CheckWindows [mkAfter RoundEnds]
    getVillagers (toTarget farmhouse) `shouldReturn` 3
    deadVillagerCount `shouldReturn` 1

  it "lets Lord Alcaron place a discarded villager on a location instead" . amonDinTest $ \pid -> do
    setUp pid
    farmhouse <- locationOf Locations.burningFarmhouse
    secluded <- locationOf Locations.secludedFarmhouse
    run $ CheckWindows [mkAfter RoundEnds]
    chooseTarget pid secluded
    getVillagers (toTarget farmhouse) `shouldReturn` 3
    getVillagers (toTarget secluded) `shouldReturn` 3
    deadVillagerCount `shouldReturn` 0
    fmap (allyExhausted . toAttrs) <$> alcaron `shouldReturn` Just True
    -- exhausted, he answers no more
    run $ CheckWindows [mkAfter RoundEnds]
    deadVillagerCount `shouldReturn` 1

  it "kills the villager when Lord Alcaron's controller declines" . amonDinTest $ \pid -> do
    setUp pid
    run $ CheckWindows [mkAfter RoundEnds]
    chooseLabel "$label.lordAlcaron.decline" pid
    deadVillagerCount `shouldReturn` 1

  it "loses the game when Lord Alcaron leaves play" . amonDinTest $ \pid -> do
    setUp pid
    a <- fromJustNote "no Alcaron" <$> alcaron
    run $ Discard (toTarget a)
    getsGame (.outcome) `shouldReturn` Just Lost

  it "places a dead villager as Ghulat attacks, and attacks for the dead villagers" . amonDinTest $ \pid -> do
    _ <- objectives 0
    ghulat <- engaged pid Enemies.ghulat
    getEnemyAttack ghulat `shouldReturn` 0
    run $ EnemyAttack pid ghulat
    deadVillagerCount `shouldReturn` 1
    getEnemyAttack ghulat `shouldReturn` 1

  it "discards rescued villagers for an undefended attack at Protect the Villagers" . amonDinTest $ \pid -> do
    _ <- objectives 5
    quest <- putQuestStage Quests.protectTheVillagers
    run $ Flip quest
    resolveWhenRevealed pid
    orc <- engaged pid Enemies.maraudingOrc
    run $ UndefendedDamage pid orc
    rescuedVillagerCount `shouldReturn` 1
    deadVillagerCount `shouldReturn` 4
    (getDamage . toTarget =<< getHeroWithCode "01007") `shouldReturn` 0

  it "cannot end the game while Ghulat is in play, then compares the villagers" . amonDinTest $ \pid -> do
    _ <- objectives 3
    quest <- putQuestStage Quests.protectTheVillagers
    run $ Flip quest
    resolveWhenRevealed pid
    run . AddToStaging =<< genEncounterCard Enemies.ghulat
    run $ PlaceTokens GameSource quest Progress 15
    getsGame (.outcome) `shouldReturn` Nothing
    run . Destroy GameSource . toTarget =<< enemyOf Enemies.ghulat
    getsGame (.outcome) `shouldReturn` Just Won

  it "loses at the end of the game unless more villagers were rescued than died" . amonDinTest $ \_ -> do
    _ <- objectives 1
    dead <- objectiveTarget Objectives.deadVillagers
    run $ DealDamage GameSource dead 1
    quest <- putQuestStage Quests.protectTheVillagers
    run $ PlaceTokens GameSource quest Progress 15
    getsGame (.outcome) `shouldReturn` Just Lost

  it "kills a villager after Marauding Orc destroys a character, and discards one with Trapped Inside's shadow" . amonDinTest $ \pid -> do
    _ <- objectives 2
    orc <- engaged pid Enemies.maraudingOrc
    _ <- setEncounterDeck [Treacheries.trappedInside]
    run $ DealShadowCard orc
    run $ UndefendedAttack pid orc
    passActions pid
    eowyn <- getHeroWithCode "01007"
    chooseTarget pid eowyn
    passActions pid
    rescuedVillagerCount `shouldReturn` 1
    deadVillagerCount `shouldReturn` 2

  it "discards villagers from the active location with Trapped Inside, surging when there are none" . amonDinTest $ \_ -> do
    _ <- objectives 0
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.secludedFarmhouse
    reveal [Treacheries.trappedInside, Locations.gondorianHamlet]
    (getVillagers . toTarget =<< locationOf Locations.secludedFarmhouse) `shouldReturn` 1
    deadVillagerCount `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    reveal [Enemies.orcRavager, Treacheries.trappedInside]
    reveal [Treacheries.trappedInside, Locations.gondorianHamlet]
    getsGame (length . (.entities.locations)) `shouldReturn` 2

  it "moves rescued villagers to a location in the staging area with Panicked!" . amonDinTest $ \_ -> do
    _ <- objectives 2
    secluded <- stage Locations.secludedFarmhouse
    reveal [Treacheries.panicked]
    rescuedVillagerCount `shouldReturn` 1
    getVillagers (toTarget secluded) `shouldReturn` 3

  it "discards a villager from Gondorian Hamlet in the staging area as a treachery is revealed" . amonDinTest $ \_ -> do
    _ <- objectives 1
    hamlet <- stage Locations.gondorianHamlet
    reveal [Treacheries.panicked]
    -- Panicked! moved a villager onto it, then it lost one
    getVillagers (toTarget hamlet) `shouldReturn` 3
    deadVillagerCount `shouldReturn` 1

  it "raises each player's threat by the dead villagers with Burnt Homestead" . amonDinTest $ \pid -> do
    _ <- objectives 0
    dead <- objectiveTarget Objectives.deadVillagers
    run $ DealDamage GameSource dead 2
    threat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.burntHomestead, Locations.secludedFarmhouse]
    (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  it "discards the character with the fewest remaining hit points with Craven Eagle" . amonDinTest $ \pid -> do
    hand <- length . (.hand) <$> getPlayer pid
    reveal [Enemies.cravenEagle]
    -- with 3 cards in hand Éowyn's player may discard them at random instead
    when (hand >= 3) $ chooseLabel "$label.cravenEagle.discardCharacter" pid
    getsGame (any ((== "01007") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` False

  it "keeps the character when its player discards 3 cards at random for Craven Eagle" . amonDinTest $ \pid -> do
    for_ [1 .. 3 :: Int] \_ -> putCardIntoHand pid Allies.guardOfTheCitadel
    hand <- length . (.hand) <$> getPlayer pid
    reveal [Enemies.cravenEagle]
    chooseLabel "$label.cravenEagle.discardCards" pid
    getsGame (any ((== "01007") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` True
    length . (.hand) <$> getPlayer pid `shouldReturn` hand - 3
