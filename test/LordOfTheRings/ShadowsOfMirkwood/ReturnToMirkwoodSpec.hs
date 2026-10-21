module LordOfTheRings.ShadowsOfMirkwood.ReturnToMirkwoodSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (returnToMirkwood)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

mirkwoodTest :: (PlayerId -> GameT ()) -> IO ()
mirkwoodTest = gameTestIn returnToMirkwood testDeck

gollum :: GameT Target
gollum = getsGame (toTarget . fromJustNote "no Gollum" . find ((== toCardCode Objectives.gollumReturnToMirkwood) . toCardCode) . toList . (.entities.objectives))

-- | Gollum guarded by the player.
guardGollum :: PlayerId -> GameT Target
guardGollum pid = do
  run . AddToStaging =<< genEncounterCard Objectives.gollumReturnToMirkwood
  g <- gollum
  case g of
    ObjectiveTarget oid -> run $ PlaceObjective oid (InPlayArea pid)
    _ -> pure ()
  pure g

offers :: PlayerId -> Target -> GameT Bool
offers pid t = maybe False (any \case TargetLabel t' _ -> t == t'; _ -> False) . fmap questionChoices <$> getQuestion pid

spec :: Spec
spec = describe "Return to Mirkwood" do
  it "sets Gollum in front of the player chosen to guard him" . mirkwoodTest $ \pid -> do
    flipFirstQuest pid
    g <- gollum
    getsGame (fmap (.placement) . find ((== g) . toTarget) . toList . (.entities.objectives)) `shouldReturn` Just (InPlayArea pid)

  it "raises the guard's threat by 3 at the end of each round" . mirkwoodTest $ \pid -> do
    _ <- guardGollum pid
    threat <- (.threat) <$> getPlayer pid
    run EndRound
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3

  it "deals undefended damage against the guard to Gollum, and loses when he is destroyed" . mirkwoodTest $ \pid -> do
    _ <- guardGollum pid
    _ <- setEncounterDeck [Enemies.attercopAttercop]
    run RevealEncounterCard
    attercop <- getsGame (fromJustNote "no Attercop" . headMay . map toId . toList . (.entities.enemies))
    run $ UndefendedDamage pid attercop
    getsGame (.outcome) `shouldReturn` Just Lost

  it "raises the guard's threat by 8 with Gollum's Anguish" . mirkwoodTest $ \pid -> do
    _ <- guardGollum pid
    threat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.gollumsAnguish]
    run RevealEncounterCard
    resolveRevealed
    (.threat) <$> getPlayer pid `shouldReturn` threat + 8

  it "loses at Escape Attempt when the players quest unsuccessfully" . mirkwoodTest $ \pid -> do
    _ <- guardGollum pid
    _ <- putQuestStage Quests.escapeAttempt
    _ <- setEncounterDeck [Enemies.attercopAttercop]
    run RevealEncounterCard
    run QuestResolution
    getsGame (.outcome) `shouldReturn` Just Lost

  it "keeps the guard from playing cards at To the Elven King's Halls" . mirkwoodTest $ \pid -> do
    _ <- guardGollum pid
    _ <- putQuestStage Quests.toTheElvenKingsHalls
    card <- putCardIntoHand pid Events.shadowOfThePast
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 2
    run $ BeginPhase PlanningPhase
    offers pid (toTarget card) `shouldReturn` False

  it "has Attercop, Attercop engage the guard as the encounter phase begins" . mirkwoodTest $ \pid -> do
    _ <- guardGollum pid
    _ <- setEncounterDeck [Enemies.attercopAttercop]
    run RevealEncounterCard
    run $ BeginPhase EncounterPhase
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [EngagedWith pid]

  it "puts a departed Eagle facedown under Eagles of the Misty Mountains" . mirkwoodTest $ \pid -> do
    eagles <- putAllyIntoPlay pid Allies.eaglesOfTheMistyMountains
    passActions pid
    guardian <- putAllyIntoPlay pid Allies.wingedGuardian
    passActions pid
    run $ Discard guardian
    chooseAbilityOf pid eagles
    getsGame (fmap (length . allyFacedown . toAttrs) . find ((== eagles) . toTarget) . toList . (.entities.allies)) `shouldReturn` Just 1
