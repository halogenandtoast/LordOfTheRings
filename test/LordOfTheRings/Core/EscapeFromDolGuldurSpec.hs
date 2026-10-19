module LordOfTheRings.Core.EscapeFromDolGuldurSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Hero (heroPrisoner)
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

escapeTest :: (PlayerId -> GameT ()) -> IO ()
escapeTest = gameTestIn escapeFromDolGuldur starterDeck

prisoners :: GameT [Hero]
prisoners = getsGame (filter (heroPrisoner . toAttrs) . toList . (.entities.heroes))

-- | An objective in the staging area that a treachery left free to claim.
freeObjective :: CardDef -> GameT Target
freeObjective def = do
  _ <- setEncounterDeck [Treacheries.theNecromancersReach]
  card <- genEncounterCard def
  run $ PutObjectiveIntoPlay card
  resolveRevealed
  getsGame (toTarget . fromJustNote "no objective" . find ((== card.id) . objectiveCardId . toAttrs) . toList . (.entities.objectives))

claimOnto :: PlayerId -> Target -> GameT ()
claimOnto pid objective = do
  run $ BeginPhase PlanningPhase
  chooseAbilityOf pid objective
  chooseTarget pid . toTarget =<< getHeroWithCode "01001"
  passActions pid

spec :: Spec
spec = describe "Escape from Dol Guldur" do
  it "sets up the objectives, sets the Nazgul aside and takes a hero prisoner" . escapeTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    chooseOptionMatching "flip the quest" pid (const True)
    -- the objectives' guards, then the stage
    resolveRevealed
    getsGame (length . (.entities.objectives)) `shouldReturn` 3
    map toCardCode . scenarioSetAside . toAttrs <$> getsGame (.scenario) `shouldReturn` [toCardCode Enemies.nazgulOfDolGuldur]
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` notElem (toCardCode Enemies.nazgulOfDolGuldur))
    length <$> prisoners `shouldReturn` 1

  it "lets the group play only 1 ally each round" . escapeTest $ \pid -> do
    _ <- putQuestStage Quests.theNecromancersTower
    second <- putCardIntoHand pid Allies.guardOfTheCitadel
    _ <- playWith pid "01001" 4 Allies.guardOfTheCitadel
    passActions pid
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    [() | Just q <- [question], TargetLabel (CardIdTarget cid) _ <- questionChoices q, cid == second.id] `shouldBe` []

  it "holds the players at The Necromancer's Tower until they claim an objective" . escapeTest $ \pid -> do
    stage <- putQuestStage Quests.theNecromancersTower
    run $ PlaceTokens GameSource stage Progress 9
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.theNecromancersTower
    claimOnto pid =<< freeObjective Objectives.gandalfsMap
    -- defeated: with no stage after it here, the quest is over
    getsGame (length . (.entities.quests)) `shouldReturn` 0

  it "rescues the prisoner at Through the Caverns, and the Nazgul comes" . escapeTest $ \pid -> do
    run . SetAside =<< genEncounterCard Enemies.nazgulOfDolGuldur
    eowyn <- getHeroWithCode "01003"
    run $ TakePrisoner eowyn.id
    stage <- putQuestStage Quests.throughTheCaverns
    run $ PlaceTokens GameSource stage Progress 1
    chooseAbilityOf pid stage
    length <$> prisoners `shouldReturn` 0
    (.damage) <$> getHeroWithCode "01003" `shouldReturn` 1
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.nazgulOfDolGuldur]

  it "keeps a prisoner from being used" . escapeTest $ \pid -> do
    gloin <- getHeroWithCode "01003"
    run $ TakePrisoner gloin.id
    run $ BeginPhase QuestPhase
    Just question <- getQuestion pid
    [() | TargetLabel t _ <- questionChoices question, t == toTarget gloin] `shouldBe` []

  it "sends each player an Orc Guard at the start of each quest phase at Out of the Dungeons" . escapeTest $ \pid -> do
    _ <- putQuestStage Quests.outOfTheDungeons
    deck <- length . (.deck) <$> getPlayer pid
    run $ BeginPhase QuestPhase
    guards <- getsGame (map toId . filter ((== "OrcGuard") . toCardCode) . toList . (.entities.enemies))
    length guards `shouldBe` 1
    length . (.deck) <$> getPlayer pid `shouldReturn` deck - 1
    chooseLabel "$label.doneCommitting" pid
    for_ guards \eid -> run $ DealDamage GameSource (toTarget eid) 1
    length . (.discard) <$> getPlayer pid `shouldReturn` 1
