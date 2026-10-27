module LordOfTheRings.Dwarrowdelf.TheRedhornGateSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo, CannotCommitToQuest, DoesNotCountWillpower))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theRedhornGate)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), encounterDiscardL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

redhornTest :: (PlayerId -> GameT ()) -> IO ()
redhornTest = gameTestIn theRedhornGate testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
      locations = [l.placement | l <- toList g.entities.locations, toCardCode l == code]
      enemies = [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
      allies = [allyPlacement (toAttrs a) | a <- toList g.entities.allies, toCardCode a == code]
  pure $ headMay (locations <> enemies <> allies)

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

setEncounterDiscard :: [CardDef] -> GameT ()
setEncounterDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

heroInPlay :: Target -> GameT Bool
heroInPlay = \case
  HeroTarget hid -> getsGame (member hid . (.entities.heroes))
  _ -> pure False

freezingColds :: GameT Int
freezingColds = getsGame (length . filter ((== toCardCode Treacheries.freezingCold) . toCardCode) . toList . (.entities.treacheries))

spec :: Spec
spec = describe "The Redhorn Gate" do
  it "stages Caradhras, gives Arwen to the first player and sets every Snowstorm aside" . redhornTest $ \pid -> do
    flipFirstQuest pid
    placementOf Locations.caradhras `shouldReturn` Just StagingArea
    placementOf Objectives.arwenUndomielTheRedhornGate `shouldReturn` Just (InPlayArea pid)
    setAside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    copies Treacheries.snowstorm setAside `shouldBe` 5
    copies Treacheries.snowstorm <$> getEncounterDeck `shouldReturn` 0

  it "lowers questing willpower while Caradhras is active, and cannot be traveled to" . redhornTest $ \_ -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.caradhras
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ CommitToQuest eowyn
    getWillpower eowyn `shouldReturn` 3
    caradhras <- locationOf Locations.caradhras
    getModifiers (LocationTarget caradhras) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  it "shuffles one more Snowstorm than players in at Snowdrifts" . redhornTest $ \pid -> do
    for_ [1 :: Int .. 5] . const $ run . SetAside =<< genEncounterCard Treacheries.snowstorm
    quest <- putQuestStage Quests.snowdrifts
    run $ Flip quest
    resolveWhenRevealed pid
    copies Treacheries.snowstorm <$> getEncounterDeck `shouldReturn` 2
    copies Treacheries.snowstorm <$> getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 3

  it "makes Caradhras the active location as the 11th progress lands on Snowdrifts" . redhornTest $ \_ -> do
    quest <- putQuestStage Quests.snowdrifts
    run . AddToStaging =<< genEncounterCard Locations.caradhras
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.rockyCrags
    run $ PlaceTokens GameSource quest Progress 11
    placementOf Locations.caradhras `shouldReturn` Just ActiveLocation
    placementOf Locations.rockyCrags `shouldReturn` Nothing

  it "discards a character whose willpower drops to 0 at The Mountains' Peaks" . redhornTest $ \_ -> do
    _ <- putQuestStage Quests.theMountainsPeaks
    theodred <- toTarget <$> getHeroWithCode "01002"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ CommitToQuest theodred
    run $ CommitToQuest eowyn
    reveal [Treacheries.snowstorm]
    heroInPlay theodred `shouldReturn` False
    heroInPlay eowyn `shouldReturn` True

  it "wins The Mountains' Peaks only with 5 victory points" . redhornTest $ \_ -> do
    quest <- putQuestStage Quests.theMountainsPeaks
    run $ PlaceTokens GameSource quest Progress 13
    getsGame (.outcome) `shouldReturn` Nothing
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.caradhras
    run . AddToVictoryDisplay . LocationTarget =<< locationOf Locations.caradhras
    getsGame (.outcome) `shouldReturn` Nothing
    run . AddToStaging =<< genEncounterCard Locations.fanuidhol
    run . AddToVictoryDisplay . LocationTarget =<< locationOf Locations.fanuidhol
    getsGame (.outcome) `shouldReturn` Just Won

  it "keeps a hero with Freezing Cold from questing, and discards it with a second copy" . redhornTest $ \pid -> do
    theodred <- toTarget <$> getHeroWithCode "01002"
    reveal [Treacheries.freezingCold]
    chooseTarget pid theodred
    getModifiers theodred >>= (`shouldSatisfy` elem CannotCommitToQuest)
    heroInPlay theodred `shouldReturn` True
    reveal [Treacheries.freezingCold]
    chooseTarget pid theodred
    heroInPlay theodred `shouldReturn` False

  it "keeps allies from defending Snow Warg, and damages its defender" . redhornTest $ \pid -> do
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    reveal [Enemies.snowWarg]
    warg <- enemyOf Enemies.snowWarg
    run $ EngageEnemy pid warg
    run $ EnemyAttack pid warg
    passActions pid
    offered <- maybe [] questionChoices <$> getQuestion pid
    [() | TargetLabel t _ <- offered, t == guard'] `shouldBe` []
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    -- 1 from its Forced effect, 1 from its 3 attack against her 2 defense
    getCharacterDamage beravor `shouldReturn` 2

  it "gives Mountain Goblin +1 attack for each Mountain location in the staging area" . redhornTest $ \_ -> do
    reveal [Enemies.mountainGoblin]
    goblin <- enemyOf Enemies.mountainGoblin
    run . AddToStaging =<< genEncounterCard Locations.rockyCrags
    run . AddToStaging =<< genEncounterCard Locations.caradhras
    getEnemyAttack goblin `shouldReturn` 4

  it "returns 2 Snow treacheries with Fell Voices, without surging" . redhornTest $ \_ -> do
    _ <- setEncounterDeck [Treacheries.fellVoices, Enemies.mountainTroll]
    setEncounterDiscard [Treacheries.snowstorm, Enemies.mountainGoblin, Treacheries.freezingCold]
    run RevealEncounterCard
    resolveRevealed
    map toCardCode <$> getEncounterDeck
      `shouldReturn` map toCardCode [Treacheries.snowstorm, Treacheries.freezingCold, Enemies.mountainTroll]

  it "surges Fell Voices when it returns fewer than 2 Snow treacheries" . redhornTest $ \_ -> do
    _ <- setEncounterDeck [Treacheries.fellVoices]
    setEncounterDiscard [Enemies.snowWarg]
    run RevealEncounterCard
    resolveRevealed
    placementOf Enemies.snowWarg `shouldReturn` Just StagingArea

  it "reshuffles locations, lowers threat by 11 and thaws Freezing Cold as the players travel to The Dimrill Stair" . redhornTest $ \pid -> do
    theodred <- toTarget <$> getHeroWithCode "01002"
    reveal [Treacheries.freezingCold]
    chooseTarget pid theodred
    _ <- setEncounterDeck []
    setEncounterDiscard [Locations.rockyCrags, Treacheries.snowstorm]
    peak <- EncounterCard <$> genEncounterCard Locations.caradhras
    putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (peak :))) =<< getGame
    run . AddToStaging =<< genEncounterCard Locations.theDimrillStair
    stair <- locationOf Locations.theDimrillStair
    run $ RaiseThreat pid 20
    was <- (.threat) <$> getPlayer pid
    run $ TravelTo stair
    (.threat) <$> getPlayer pid `shouldReturn` was - 11
    freezingColds `shouldReturn` 0
    deck <- getEncounterDeck
    copies Locations.rockyCrags deck `shouldBe` 1
    copies Locations.caradhras deck `shouldBe` 1

  it "removes 2 progress from Celebdil at the end of the round while it is active" . redhornTest $ \_ -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.celebdil
    celebdil <- locationOf Locations.celebdil
    run $ PlaceTokens GameSource (LocationTarget celebdil) Progress 3
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getLocation celebdil `shouldReturn` 1

  it "makes heroes pay a resource to count their willpower at Fanuidhol" . redhornTest $ \pid -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.fanuidhol
    beravor <- getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 1
    run $ CommitToQuest (toTarget beravor)
    chooseLabel "$label.fanuidhol.spend" pid
    getHeroResources (toId beravor) `shouldReturn` 0
    getModifiers (toTarget beravor) >>= (`shouldSatisfy` notElem DoesNotCountWillpower)
    run $ CommitToQuest eowyn
    getModifiers eowyn >>= (`shouldSatisfy` elem DoesNotCountWillpower)

  it "exhausts and commits every ready character with Avalanche! in the quest phase" . redhornTest $ \_ -> do
    putGame . (phaseL .~ QuestPhase) =<< getGame
    reveal [Treacheries.avalanche]
    heroes <- getsGame (toList . (.entities.heroes))
    map (\h -> (heroExhausted (toAttrs h), heroCommitted (toAttrs h))) heroes `shouldBe` map (const (True, True)) heroes
