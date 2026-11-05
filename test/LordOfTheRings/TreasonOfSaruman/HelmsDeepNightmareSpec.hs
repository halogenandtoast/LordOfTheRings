module LordOfTheRings.TreasonOfSaruman.HelmsDeepNightmareSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (helmsDeep)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

helms :: (PlayerId -> GameT ()) -> IO ()
helms = gameTestIn helmsDeep testDeck

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

resolveForcedChoices :: PlayerId -> GameT ()
resolveForcedChoices pid = getQuestion pid >>= \case
  Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> chooseAbility pid >> resolveForcedChoices pid
  _ -> pure ()

spec :: Spec
spec = describe "Helm's Deep Nightmare" do
  it "Host of Isengard has Toughness 2" . helms $ \_ -> do
    eid <- stage Enemies.hostOfIsengard
    run $ DealDamage GameSource (EnemyTarget eid) 3
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Host of Isengard's round-end progress bypasses the active location" . helms $ \_ -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    card <- genEncounterCard Locations.deepingWall
    run $ AddToStaging card
    lid <- getsGame (toId . fromJustNote "Deeping Wall" . find ((== toCardCode Locations.deepingWall) . toCardCode) . toList . (.entities.locations))
    run $ PlaceLocation lid ActiveLocation
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 1
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "Host of Isengard places progress at the end of each round" . helms $ \_ -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "Host of Isengard does not place progress while engaged" . helms $ \pid -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    eid <- stage Enemies.hostOfIsengard
    run $ EngageEnemy pid eid
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 0
    run $ PlaceEnemy eid StagingArea
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 1

  it "each staged Host of Isengard places progress" . helms $ \pid -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    _ <- stage Enemies.hostOfIsengard
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    resolveForcedChoices pid
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "blanking Host of Isengard removes its round-end ability" . helms $ \_ -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    eid <- stage Enemies.hostOfIsengard
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 1

  it "Host of Isengard cannot receive attachments" . helms $ \pid -> do
    eid <- stage Enemies.hostOfIsengard
    run $ EngageEnemy pid eid
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 3
    card <- putCardIntoHand pid Attachments.forestSnare
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player card `shouldReturn` False
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    canPlay player card `shouldReturn` True

  it "Host of Isengard adds its progress to stage 2B's ordinary round-end progress" . helms $ \pid -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    putGame . (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) =<< getGame
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    resolveForcedChoices pid
    (.progress) <$> getActiveQuest `shouldReturn` 2
