module LordOfTheRings.KhazadDum.TheSeventhLevelSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotAttack, DoesNotExhaustToCommit))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theSeventhLevel)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (StagingStepEnds), mkAfter)
import TestImport

seventhTest :: (PlayerId -> GameT ()) -> IO ()
seventhTest = gameTestIn theSeventhLevel testDeck

book :: GameT (Maybe Placement)
book = getsGame (fmap (.placement) . find ((== toCardCode Objectives.bookOfMazarbul) . toCardCode) . toList . (.entities.objectives))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . find' . toList . (.entities.enemies))
 where
  find' = fmap toId . find ((== toCardCode def) . toCardCode)

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "The Seventh Level" do
  it "has the first player attach Book of Mazarbul to a hero at setup" . seventhTest $ \pid -> do
    flipFirstQuest pid
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    book `shouldReturn` Just (AttachedTo beravor)
    mods <- getModifiers beravor
    mods `shouldSatisfy` elem CannotAttack
    mods `shouldSatisfy` elem DoesNotExhaustToCommit

  it "removes Book of Mazarbul from the game as The Fate of Balin is revealed" . seventhTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.bookOfMazarbul
    stage <- putQuestStage Quests.theFateOfBalin
    run $ WhenRevealed stage
    book `shouldReturn` Nothing
    void $ pure pid

  it "adds the enemies at the top of the deck to staging at the end of the staging step, discarding the rest" . seventhTest $ \pid -> do
    _ <- putQuestStage Quests.theFateOfBalin
    _ <- setEncounterDeck [Treacheries.hiddenThreat]
    was <- threat pid
    run $ CheckWindows [mkAfter StagingStepEnds]
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Treacheries.hiddenThreat]
    threat pid `shouldReturn` was
    _ <- setEncounterDeck [Enemies.goblinSpearman]
    run $ CheckWindows [mkAfter StagingStepEnds]
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [StagingArea]

  it "has Orc Horn Blower reveal another card, and surge" . seventhTest $ \_ -> do
    reveal [Enemies.orcHornBlower, Enemies.goblinSpearman, Enemies.goblinArcher]
    getsGame (length . (.entities.enemies)) `shouldReturn` 3

  it "raises each player's threat for each enemy in staging with Hidden Threat" . seventhTest $ \pid -> do
    reveal [Enemies.goblinSpearman]
    was <- threat pid
    reveal [Treacheries.hiddenThreat]
    threat pid `shouldReturn` was + 1

  it "gives Chieftain of the Pit +3 attack the round it is revealed" . seventhTest $ \_ -> do
    reveal [Enemies.chieftainOfThePit]
    (getEnemyAttack =<< enemyOf Enemies.chieftainOfThePit) `shouldReturn` 8

  it "gives enemies +1 attack while Plundered Armoury is in staging" . seventhTest $ \_ -> do
    reveal [Enemies.goblinArcher]
    reveal [Locations.plunderedArmoury]
    (getEnemyAttack =<< enemyOf Enemies.goblinArcher) `shouldReturn` 2

  it "deals Goblin Spearman's undefended attack with +2 attack" . seventhTest $ \pid -> do
    reveal [Enemies.goblinSpearman]
    eid <- enemyOf Enemies.goblinSpearman
    run $ EngageEnemy pid eid
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ UndefendedDamage pid eid
    chooseTarget pid beravor
    -- 4 damage destroys Beravor's 4 hit points; 2 would not
    getsGame (any ((== beravor) . toTarget) . toList . (.entities.heroes)) `shouldReturn` False

  it "has Undisturbed Bones deal an ally as much damage as its controller has allies" . seventhTest $ \pid -> do
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    passActions pid
    reveal [Treacheries.undisturbedBones]
    getDamage guard' `shouldReturn` 1

  it "claims Book of Mazarbul by exhausting a hero as its cost" . seventhTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.bookOfMazarbul
    bookId <- getsGame (fromJustNote "no book" . fmap toTarget . headMay . toList . (.entities.objectives))
    beravor <- getHeroWithCode "01012"
    run $ ActionWindow pid
    chooseAbilityOf pid bookId
    chooseTarget pid beravor
    book `shouldReturn` Just (AttachedTo $ toTarget beravor)
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "does not offer Book of Mazarbul without a hero that can exhaust" . seventhTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.bookOfMazarbul
    bookId <- getsGame (fromJustNote "no book" . fmap toTarget . headMay . toList . (.entities.objectives))
    for_ ["01007", "01002"] \code -> run . Exhaust . toTarget =<< getHeroWithCode code
    beravor <- getHeroWithCode "01012"
    -- Shadow of Fear: the hero cannot exhaust
    _ <- setEncounterDeck [Treacheries.shadowOfFear]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid beravor
    run $ ActionWindow pid
    offersAbilityOf pid bookId `shouldReturn` False
