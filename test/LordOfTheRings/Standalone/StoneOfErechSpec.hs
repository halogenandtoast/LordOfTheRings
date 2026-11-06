module LordOfTheRings.Standalone.StoneOfErechSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Character (getAttackStrength)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense)
import LordOfTheRings.Keyword (Keyword (Spectral))
import LordOfTheRings.Matcher (getObjective, getThreat)
import LordOfTheRings.Modifier (ModifierType (AttackModifier, WillpowerModifier, AttackBonusAgainst, LoseKeyword, Blank))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight, currentNightObjective)
import LordOfTheRings.Trait (Trait (Oathbreaker))
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

stageNight :: CardDef -> GameT Target
stageNight def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toTarget . fromJustNote "Night missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives)

prepareNights :: GameT Target
prepareNights = do
  for_ [Objectives.dusk, Objectives.midnight] \def -> run . SetAside =<< genEncounterCard def
  stageNight Objectives.eventide

nightProgress :: GameT Int
nightProgress = do
  oid <- fromJustNote "no current Night" <$> currentNightObjective
  (.progress) . toAttrs <$> getObjective oid

stageCaptain :: GameT EnemyId
stageCaptain = do
  run . AddToStaging =<< genEncounterCard Enemies.shadowHostCaptain
  getsGame $ toId . fromJustNote "captain missing" . headMay . toList . (.entities.enemies)

endRoundWindow :: GameT ()
endRoundWindow = run $ CheckWindows [mkWhen RoundEnds]

spec :: Spec
spec = describe "Stone of Erech" do
  describe "Night objectives" do
    it "has no current Night before the objectives enter play" . gameTest $ \_ -> do
      currentNight `shouldReturn` Nothing
      currentNightObjective `shouldReturn` Nothing

    it "progresses Eventide to Dusk after four rounds, without progressing Dusk in the same round" . gameTest $ \_ -> do
      _ <- prepareNights
      replicateM_ 3 endRoundWindow
      currentNight `shouldReturn` Just Eventide
      nightProgress `shouldReturn` 3
      endRoundWindow
      currentNight `shouldReturn` Just Dusk
      nightProgress `shouldReturn` 0
      cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      map toCardCode cards `shouldBe` [toCardCode Objectives.eventide]

    it "progresses Dusk to Midnight after eight rounds, leaving both earlier Nights in victory" . gameTest $ \_ -> do
      _ <- prepareNights
      replicateM_ 8 endRoundWindow
      currentNight `shouldReturn` Just Midnight
      nightProgress `shouldReturn` 0
      cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      sort (map toCardCode cards) `shouldBe` sort (map toCardCode [Objectives.eventide, Objectives.dusk])
      length <$> getsGame (.entities.objectives) `shouldReturn` 1

    it "advances as soon as an effect puts four or more progress on the current Night" . gameTest $ \_ -> do
      start <- prepareNights
      run $ PlaceTokens GameSource start Progress 6
      currentNight `shouldReturn` Just Dusk
      nightProgress `shouldReturn` 0

    it "raises every player's threat by Midnight's new total, only from its round-end ability" . twoPlayerTest testDeck $ \one two -> do
      midnight <- stageNight Objectives.midnight
      initial <- traverse getThreat [one, two]
      run $ PlaceTokens GameSource midnight Progress 2
      traverse getThreat [one, two] `shouldReturn` initial
      endRoundWindow
      nightProgress `shouldReturn` 3
      traverse getThreat [one, two] `shouldReturn` map (+ 3) initial
      endRoundWindow
      nightProgress `shouldReturn` 4
      traverse getThreat [one, two] `shouldReturn` map (+ 7) initial
      currentNight `shouldReturn` Just Midnight

    it "lets The Gloaming progress and replace the current Night, then resolve surge" . gameTest $ \_ -> do
      start <- prepareNights
      run $ PlaceTokens GameSource start Progress 3
      _ <- setEncounterDeck [Treacheries.theGloaming, Enemies.forestSpider]
      run RevealEncounterCard
      resolveRevealed
      currentNight `shouldReturn` Just Dusk
      nightProgress `shouldReturn` 0
      length <$> getsGame (.entities.enemies) `shouldReturn` 1
      getEncounterDiscard >>= (\cards -> map toCardCode cards `shouldBe` [toCardCode Treacheries.theGloaming])

  describe "Spectral" do
    it "uses modified willpower and ignores attack and attack-against-trait bonuses" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource eowyn [AttackModifier 10, AttackBonusAgainst Oathbreaker 5, WillpowerModifier 2] UntilEndOfRound
      getAttackStrength [eowyn] eid Enemies.shadowHostCaptain `shouldReturn` 6

    it "totals the willpower of all attackers" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      beravor <- heroOf pid "01012"
      getAttackStrength [eowyn, beravor] eid Enemies.shadowHostCaptain `shouldReturn` 6

    it "returns to attack when Spectral is lost" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource (toTarget eid) [LoseKeyword Spectral] UntilEndOfRound
      getAttackStrength [eowyn] eid Enemies.shadowHostCaptain `shouldReturn` 1

    it "removes printed Spectral when the enemy is blank" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource (toTarget eid) [Blank] UntilEndOfRound
      getAttackStrength [eowyn] eid Enemies.shadowHostCaptain `shouldReturn` 1

    it "still subtracts enemy defense in a resolved attack" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ AttackWithAgainst eowyn eid
      run $ ResolveAttack pid eid
      passActions pid
      getDamage (toTarget eid) `shouldReturn` 3

    it "does not alter direct damage" . gameTest $ \_ -> do
      eid <- stageCaptain
      run $ DealDamage GameSource (toTarget eid) 2
      getDamage (toTarget eid) `shouldReturn` 2

  it "updates Shadow Host Captain's stats as Night changes" . gameTest $ \_ -> do
    _ <- prepareNights
    eid <- stageCaptain
    getEnemyAttack eid `shouldReturn` 4
    getEnemyDefense eid `shouldReturn` 1
    replicateM_ 4 endRoundWindow
    getEnemyAttack eid `shouldReturn` 5
    getEnemyDefense eid `shouldReturn` 2
    replicateM_ 4 endRoundWindow
    getEnemyAttack eid `shouldReturn` 6
    getEnemyDefense eid `shouldReturn` 3
