module LordOfTheRings.KhazadDum.IntoThePitSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (PlayerWithThreatAtLeast))
import LordOfTheRings.Modifier (ModifierType (CannotBeOptionallyEngagedBy, QuestPointModifier))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (intoThePit)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

pitTest :: (PlayerId -> GameT ()) -> IO ()
pitTest = gameTestIn intoThePit testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
      locations = [l.placement | l <- toList g.entities.locations, toCardCode l == code]
      enemies = [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
      objectives = [o.placement | o <- toList g.entities.objectives, toCardCode o == code]
  pure $ headMay (locations <> enemies <> objectives)

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (.damage) . lookup eid . (.entities.enemies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

spec :: Spec
spec = describe "Into the Pit" do
  it "starts at East-gate with Cave Torch on the chosen hero, First Hall and the Bridge set aside" . pitTest $ \pid -> do
    flipFirstQuest pid
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    placementOf Locations.eastGate `shouldReturn` Just ActiveLocation
    placementOf Objectives.caveTorch `shouldReturn` Just (AttachedTo beravor)
    setAside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    setAside `shouldSatisfy` elem (toCardCode Locations.firstHall)
    setAside `shouldSatisfy` elem (toCardCode Locations.bridgeOfKhazadDum)

  it "adds First Hall to the staging area once East-gate is explored" . pitTest $ \_ -> do
    _ <- putQuestStage Quests.enteringTheMines
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.eastGate
    run . SetAside =<< genEncounterCard Locations.firstHall
    gate <- locationOf Locations.eastGate
    run $ PlaceTokens GameSource (toTarget gate) Progress 7
    placementOf Locations.firstHall `shouldReturn` Just StagingArea

  it "leaves East-gate active as Dreadful Gap becomes active too, East-gate being immune" . pitTest $ \_ -> do
    _ <- putQuestStage Quests.enteringTheMines
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.eastGate
    reveal [Locations.dreadfulGap]
    placementOf Locations.eastGate `shouldReturn` Just ActiveLocation
    placementOf Locations.dreadfulGap `shouldReturn` Just ActiveLocation

  it "removes the progress on the quest with Cave In, and surges when there is none" . pitTest $ \_ -> do
    quest <- putQuestStage Quests.enteringTheMines
    run $ PlaceTokens GameSource quest Progress 3
    reveal [Treacheries.caveIn]
    (.progress) <$> getActiveQuest `shouldReturn` 0
    reveal [Treacheries.caveIn, Enemies.goblinScout]
    placementOf Enemies.goblinScout `shouldReturn` Just StagingArea

  it "leaves East-gate's progress to Cave In, which surges as it removed none" . pitTest $ \_ -> do
    _ <- putQuestStage Quests.enteringTheMines
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.eastGate
    gate <- locationOf Locations.eastGate
    run $ PlaceTokens GameSource (toTarget gate) Progress 4
    reveal [Treacheries.caveIn, Enemies.goblinScout]
    (.progress) <$> getLocation gate `shouldReturn` 4
    placementOf Enemies.goblinScout `shouldReturn` Just StagingArea

  it "cancels damage to Patrol Leader when the discarded card is an enemy" . pitTest $ \_ -> do
    reveal [Enemies.patrolLeader]
    leader <- enemyOf Enemies.patrolLeader
    _ <- setEncounterDeck [Enemies.goblinScout, Treacheries.caveIn]
    run $ DealDamage GameSource (toTarget leader) 2
    enemyDamage leader `shouldReturn` 0
    run $ DealDamage GameSource (toTarget leader) 2
    enemyDamage leader `shouldReturn` 2

  it "keeps a player at 25 threat or more from optionally engaging Goblin Scout" . pitTest $ \_ -> do
    reveal [Enemies.goblinScout]
    scout <- enemyOf Enemies.goblinScout
    getModifiers (toTarget scout) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy (PlayerWithThreatAtLeast 25)))

  it "discards enemies revealed at Goblin Patrol, and advances once no enemy is in play" . pitTest $ \_ -> do
    _ <- putQuestStage Quests.goblinPatrol
    reveal [Enemies.goblinScout]
    placementOf Enemies.goblinScout `shouldReturn` Nothing
    getsGame (.outcome) `shouldReturn` Just Won

  it "travels at once to Dreadful Gap, whose quest points are the characters in play" . pitTest $ \_ -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.fouledWell
    reveal [Locations.dreadfulGap]
    placementOf Locations.dreadfulGap `shouldReturn` Just ActiveLocation
    placementOf Locations.fouledWell `shouldReturn` Just StagingArea
    gap <- locationOf Locations.dreadfulGap
    getModifiers (LocationTarget gap) >>= (`shouldSatisfy` elem (QuestPointModifier 3))

  it "surges Fouled Well when a player does not discard" . pitTest $ \pid -> do
    _ <- putCardIntoHand pid Events.sneakAttack
    reveal [Locations.fouledWell, Enemies.goblinScout]
    chooseLabel "$label.fouledWell.decline" pid
    placementOf Enemies.goblinScout `shouldReturn` Just StagingArea

  it "does not let the players leave Entering the Mines without the Bridge in the victory display" . pitTest $ \_ -> do
    quest <- putQuestStage Quests.enteringTheMines
    run $ PlaceTokens GameSource quest Progress 7
    getsGame (fmap toCardCode . headMay . toList . (.entities.quests)) `shouldReturn` Just (toCardCode Quests.enteringTheMines)
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.bridgeOfKhazadDum
    bridge <- locationOf Locations.bridgeOfKhazadDum
    run $ PlaceTokens GameSource (toTarget bridge) Progress 3
    getsGame (fmap toCardCode . headMay . toList . (.entities.quests)) >>= (`shouldSatisfy` (/= Just (toCardCode Quests.enteringTheMines)))

  it "has Goblin Follower engage the last player" . pitTest $ \pid -> do
    reveal [Enemies.goblinFollower]
    placementOf Enemies.goblinFollower `shouldReturn` Just (EngagedWith pid)

  it "discards the encounter deck's top as Cave Torch exhausts, adding an enemy to the staging area" . pitTest $ \_ -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.caveTorch
    torch <- getsGame (fromJustNote "no torch" . fmap toTarget . headMay . toList . (.entities.objectives))
    _ <- setEncounterDeck [Enemies.goblinScout]
    run $ Exhaust torch
    placementOf Enemies.goblinScout `shouldReturn` Just StagingArea

  it "cannot have Sudden Pitfall's when revealed effect canceled" . pitTest $ \pid -> do
    card <- putCardIntoHand pid Events.aTestOfWill
    eowyn <- getHeroWithCode "01007"
    run $ PlaceTokens GameSource (toTarget eowyn) Resource 1
    _ <- setEncounterDeck [Treacheries.suddenPitfall]
    run RevealEncounterCard
    offered <- maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices <$> getQuestion pid
    offered `shouldBe` False

  it "has Cave Torch place the amount of progress its player picks, up to 3" . pitTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.caveTorch
    torch <- getsGame (fromJustNote "no torch" . fmap toTarget . headMay . toList . (.entities.objectives))
    run . AddToStaging =<< genEncounterCard Locations.lightlessPassage
    _ <- setEncounterDeck [Treacheries.suddenPitfall]
    run $ ActionWindow pid
    chooseAbilityOf pid torch
    amounts <- maybe [] (map \case Label l _ -> l; _ -> "") . fmap questionChoices <$> getQuestion pid
    amounts `shouldBe` ["$label.amount n=i:" <> tshow n | n <- [0 .. 3 :: Int]]
    chooseLabel "$label.amount n=i:2" pid
    passage <- locationOf Locations.lightlessPassage
    getsGame (fmap (.progress) . lookup passage . (.entities.locations)) `shouldReturn` Just 2
