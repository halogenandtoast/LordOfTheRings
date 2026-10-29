module LordOfTheRings.Dwarrowdelf.RoadToRivendellSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Keyword (Keyword (Ambush))
import LordOfTheRings.Modifier (ModifierType (CannotBeHealed, CannotCancelEncounterEffects, GainKeyword))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (roadToRivendell)
import LordOfTheRings.Scenario.Types (encounterDiscardL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

rivendellTest :: (PlayerId -> GameT ()) -> IO ()
rivendellTest = gameTestIn roadToRivendell testDeck

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyPlacement :: CardDef -> GameT (Maybe Placement)
enemyPlacement def = getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationPlacement :: CardDef -> GameT (Maybe Placement)
locationPlacement def = getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

shadowCount :: EnemyId -> GameT Int
shadowCount eid = getsGame (maybe 0 (length . (.shadowCards)) . lookup eid . (.entities.enemies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

activeLocation :: CardDef -> GameT ()
activeLocation def = run . PutIntoPlayAsActiveLocation =<< genEncounterCard def

spec :: Spec
spec = describe "Road to Rivendell" do
  it "puts Arwen Undómiel into play under the first player as the setup resolves" . rivendellTest $ \pid -> do
    flipFirstQuest pid
    allies <- getsGame (toList . (.entities.allies))
    map toCardCode allies `shouldSatisfy` elem (toCardCode Objectives.arwenUndomielRoadToRivendell)
    map (.placement) allies `shouldSatisfy` elem (InPlayArea pid)

  it "has an ambusher engage the first player whose threat reaches its engagement cost" . rivendellTest $ \pid -> do
    run $ RaiseThreat pid 27
    reveal [Enemies.goblinTaskmaster]
    enemyPlacement Enemies.goblinTaskmaster `shouldReturn` Just (EngagedWith pid)
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2

  it "leaves an ambusher in the staging area while threat is below its engagement cost" . rivendellTest $ \pid -> do
    run $ RaiseThreat pid 27
    reveal [Enemies.wildBear]
    enemyPlacement Enemies.wildBear `shouldReturn` Just StagingArea

  it "has Wild Bear attack as it engages" . rivendellTest $ \pid -> do
    run $ RaiseThreat pid 34
    _ <- setEncounterDeck [Enemies.wildBear, Locations.ruinedRoad]
    run RevealEncounterCard
    enemyPlacement Enemies.wildBear `shouldReturn` Just (EngagedWith pid)
    bear <- enemyOf Enemies.wildBear
    shadowCount bear `shouldReturn` 1

  it "ignores ambush while Barren Hills is the active location" . rivendellTest $ \pid -> do
    run $ RaiseThreat pid 27
    activeLocation Locations.barrenHills
    reveal [Enemies.orcRaiders]
    enemyPlacement Enemies.orcRaiders `shouldReturn` Just StagingArea

  it "gives the first enemy revealed each round ambush at Goblin Gate, and an immediate attack" . rivendellTest $ \pid -> do
    run $ RaiseThreat pid 27
    activeLocation Locations.goblinGate
    _ <- setEncounterDeck [Enemies.goblinSpearman, Locations.ruinedRoad, Enemies.goblinSpearman]
    run RevealEncounterCard
    enemyPlacement Enemies.goblinSpearman `shouldReturn` Just (EngagedWith pid)
    spearman <- enemyOf Enemies.goblinSpearman
    shadowCount spearman `shouldReturn` 1

  it "lets only the first enemy revealed in a round gain ambush at Goblin Gate" . rivendellTest $ \pid -> do
    run $ RaiseThreat pid 27
    activeLocation Locations.goblinGate
    reveal [Enemies.wildBear]
    reveal [Enemies.goblinSpearman]
    enemyPlacement Enemies.goblinSpearman `shouldReturn` Just StagingArea

  it "counts an enemy revealed before Goblin Gate became active as the round's first" . rivendellTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.goblinGate
    gate <- locationOf Locations.goblinGate
    reveal [Enemies.goblinSpearman]
    run $ PlaceLocation gate ActiveLocation
    run $ RaiseThreat pid 27
    reveal [Enemies.goblinSpearman]
    getsGame (map (.placement) . filter ((== toCardCode Enemies.goblinSpearman) . toCardCode) . toList . (.entities.enemies))
      `shouldReturn` [StagingArea, StagingArea]

  it "makes no immediate attack at Goblin Gate for an enemy given ambush by something else" . rivendellTest $ \pid -> do
    activeLocation Locations.goblinGate
    run . AddToStaging =<< genEncounterCard Enemies.goblinSpearman
    spearman <- enemyOf Enemies.goblinSpearman
    run $ CreateEffect $ Effect GameSource (toTarget spearman) [GainKeyword Ambush] UntilEndOfRound
    run $ EngageEnemy pid spearman
    shadowCount spearman `shouldReturn` 0

  it "removes 1 progress placed on Pathless Country" . rivendellTest $ \_ -> do
    activeLocation Locations.pathlessCountry
    country <- locationOf Locations.pathlessCountry
    run $ PlaceTokens GameSource (toTarget country) Progress 2
    ((.progress) <$> getLocation country) `shouldReturn` 1

  it "has Orc Ambush engage the staging area's Orcs with the first player" . rivendellTest $ \pid -> do
    reveal [Enemies.goblinArcher]
    reveal [Treacheries.orcAmbush, Locations.ruinedRoad]
    enemyPlacement Enemies.goblinArcher `shouldReturn` Just (EngagedWith pid)

  it "has Orc Ambush return the discarded Orcs when none are in the staging area" . rivendellTest $ \_ -> do
    archer <- genEncounterCard Enemies.goblinArcher
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [archer])
    reveal [Treacheries.orcAmbush, Locations.ruinedRoad]
    enemyPlacement Enemies.goblinArcher `shouldReturn` Just StagingArea

  it "damages exhausted characters, then exhausts the ready ones, with Sleeping Sentry" . rivendellTest $ \_ -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ Exhaust beravor
    reveal [Treacheries.sleepingSentry]
    getDamage beravor `shouldReturn` 1
    getDamage eowyn `shouldReturn` 0
    getsGame (all (heroExhausted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` True

  it "surges Followed by Night once the first player damages the allies" . rivendellTest $ \pid -> do
    _ <- setEncounterDeck [Treacheries.followedByNight, Enemies.goblinArcher]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.followedByNight.damage" pid
    enemyPlacement Enemies.goblinArcher `shouldReturn` Just StagingArea

  it "keeps encounter effects from being canceled while Crebain is in the staging area" . rivendellTest $ \_ -> do
    reveal [Enemies.crebain, Locations.ruinedRoad]
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)

  it "makes Goblin Gate the active location as Orc Outpost is revealed" . rivendellTest $ \pid -> do
    _ <- setEncounterDeck [Locations.goblinGate, Locations.ruinedRoad]
    quest <- genEncounterCard Quests.orcOutpost
    run $ PutQuestIntoPlay quest
    run $ Flip (toTarget quest)
    resolveWhenRevealed pid
    locationPlacement Locations.goblinGate `shouldReturn` Just ActiveLocation

  it "keeps characters from being healed at Approaching Rivendell, and wins as it is defeated" . rivendellTest $ \_ -> do
    quest <- putQuestStage Quests.approachingRivendell
    beravor <- toTarget <$> getHeroWithCode "01012"
    getModifiers beravor >>= (`shouldSatisfy` elem CannotBeHealed)
    run $ PlaceTokens GameSource quest Progress 13
    getsGame (.outcome) `shouldReturn` Just Won
