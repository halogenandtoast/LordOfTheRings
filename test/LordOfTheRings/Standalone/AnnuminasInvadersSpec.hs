module LordOfTheRings.Standalone.AnnuminasInvadersSpec (spec, mainStage) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Matcher (getMainQuestProgress, getEnemy, getHero, getAlly)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, strongDefender, defendAttack, passAttackActions)
import TestImport

mainStage :: Int -> GameT Target
mainStage progress = do
  card <- genEncounterCard Quests.runningOutOfTime
  run $ PutQuestIntoPlay card
  target <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource target Progress progress
  pure target

invaderTest :: (PlayerId -> GameT ()) -> IO ()
invaderTest body = gameTest $ \pid -> run (DoStep 2 Setup) >> body pid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getHero hid
  AllyTarget aid -> allyExhausted . toAttrs <$> getAlly aid
  _ -> error "expected a character"

spec :: Spec
spec = describe "Annúminas invaders" do
  describe "Warg of Carn Dûm" do
    for_ [False, True] $ \removeProgress ->
      it ("makes only the chosen engagement effect happen, remove progress: " <> show removeProgress) . invaderTest $ \pid -> do
        _ <- mainStage 2
        eid <- stageEnemy Enemies.wargOfCarnDum
        hero <- heroOf pid "01012"
        run $ EngageEnemy pid eid
        if removeProgress
          then chooseLabel "$label.annuminas.removeProgress count=i:1" pid
          else chooseTarget pid hero
        getMainQuestProgress `shouldReturn` if removeProgress then 1 else 2
        isExhausted hero `shouldReturn` not removeProgress

    it "requires an exhaustable character when there is no progress and cannot exhaust another player's character" . twoPlayerTest testDeck $ \one two -> do
      _ <- mainStage 0
      eid <- stageEnemy Enemies.wargOfCarnDum
      hero <- heroOf one "01012"
      other <- heroOf two "01012"
      run $ CreateEffect $ Effect GameSource hero [CannotExhaust] UntilEndOfRound
      run $ EngageEnemy one eid
      question <- fromJustNote "missing engagement choice" <$> getQuestion one
      let targets = [target | TargetLabel target _ <- questionChoices question]
      targets `shouldSatisfy` notElem hero
      targets `shouldSatisfy` notElem other

  describe "Angmar Warg-rider" do
    for_ [False, True] $ \removeProgress ->
      it ("chooses between two progress and a fresh-shadow immediate attack: " <> show removeProgress) . invaderTest $ \pid -> do
        _ <- mainStage 3
        eid <- stageEnemy Enemies.angmarWargRider
        defender <- strongDefender pid
        _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
        run $ EngageEnemy pid eid
        chooseLabel (if removeProgress then "$label.annuminas.removeProgress count=i:2" else "$label.annuminas.immediateAttack") pid
        unless removeProgress $ defendAttack pid defender
        getMainQuestProgress `shouldReturn` if removeProgress then 1 else 3
        length <$> getEncounterDeck `shouldReturn` if removeProgress then 2 else 1

    it "does not offer a partial progress payment and resolves as much as possible if an attack is barred" . invaderTest $ \pid -> do
      _ <- mainStage 1
      eid <- stageEnemy Enemies.angmarWargRider
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
      run $ EngageEnemy pid eid
      getMainQuestProgress `shouldReturn` 0
      getQuestion pid `shouldReturn` Nothing

  describe "Orc of Angmar" do
    for_ [False, True] $ \removeProgress ->
      it ("chooses progress or defense and keeps the defense boost until the phase ends: " <> show removeProgress) . invaderTest $ \pid -> do
        _ <- mainStage 2
        eid <- stageEnemy Enemies.orcOfAngmar
        run $ EngageEnemy pid eid
        defender <- strongDefender pid
        run $ EnemyAttack pid eid
        chooseLabel (if removeProgress then "$label.annuminas.removeProgress count=i:1" else "$label.annuminas.defenseTwo") pid
        defendAttack pid defender
        getMainQuestProgress `shouldReturn` if removeProgress then 1 else 2
        getEnemyDefense eid `shouldReturn` if removeProgress then 2 else 4
        run $ EndEffects UntilEndOfPhase
        getEnemyDefense eid `shouldReturn` 2

  describe "Savage Werewolf" do
    for_ [False, True] $ \destroyed ->
      it ("removes progress only after its attack destroys a character: " <> show destroyed) . invaderTest $ \pid -> do
        _ <- mainStage 5
        eid <- stageEnemy Enemies.savageWerewolf
        run $ EngageEnemy pid eid
        defender <- if destroyed then putAllyIntoPlay pid Allies.guardOfTheCitadel else strongDefender pid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        getMainQuestProgress `shouldReturn` if destroyed then 2 else 5

  describe "Host of Angmar" do
    for_ [False, True] $ \removeProgress ->
      it ("offers its progress-or-staging choice after killing a defender: " <> show removeProgress) . invaderTest $ \pid -> do
        _ <- mainStage 3
        eid <- stageEnemy Enemies.hostOfAngmar
        run $ EngageEnemy pid eid
        defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
        run $ EnemyAttack pid eid
        defendAttack pid defender
        chooseLabel (if removeProgress then "$label.annuminas.removeProgress count=i:2" else "$label.annuminas.returnToStaging") pid
        passAttackActions
        getMainQuestProgress `shouldReturn` if removeProgress then 1 else 3
        (.placement) . toAttrs <$> getEnemy eid `shouldReturn` if removeProgress then EngagedWith pid else StagingArea

    it "is immune to player-card damage but still takes damage from the game" . invaderTest $ \pid -> do
      eid <- stageEnemy Enemies.hostOfAngmar
      HeroTarget hid <- heroOf pid "01012"
      run $ DealDamage (HeroSource hid) (EnemyTarget eid) 1
      getDamage (EnemyTarget eid) `shouldReturn` 0
      run $ DealDamage GameSource (EnemyTarget eid) 1
      getDamage (EnemyTarget eid) `shouldReturn` 1

  describe "Vanguard of Carn Dûm" do
    it "limits progress only while staged and stops its printed restrictions while blank" . invaderTest $ \pid -> do
      stage <- mainStage 0
      eid <- stageEnemy Enemies.vanguardOfCarnDum
      getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)
      run $ PlaceTokens GameSource stage Progress 8
      getMainQuestProgress `shouldReturn` 5
      run $ PlaceTokens GameSource stage Progress 8
      getMainQuestProgress `shouldReturn` 5
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource stage Progress 3
      getMainQuestProgress `shouldReturn` 8
      run $ EngageEnemy pid eid
      run $ PlaceTokens GameSource stage Progress 8
      getMainQuestProgress `shouldReturn` 16
      run $ PlaceEnemy eid StagingArea
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      getModifiers eid >>= (`shouldSatisfy` notElem CannotHaveAttachments)
      run $ PlaceTokens GameSource stage Progress 1
      getMainQuestProgress `shouldReturn` 17

  describe "shadows" do
    it "keeps Orc of Angmar's defense penalty through the round" . invaderTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      hero <- heroOf pid "01012"
      _ <- putAttachmentOn pid Attachments.citadelPlate hero
      _ <- setEncounterDeck [Enemies.orcOfAngmar, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid hero
      getDamage hero `shouldReturn` 3
      run $ DiscardShadowCards eid
      run $ Ready hero
      run $ EnemyAttack pid eid
      defendAttack pid hero
      getDamage hero `shouldReturn` 6
      getModifiers hero >>= (`shouldSatisfy` elem (DefenseModifier (-2)))
      run $ EndEffects UntilEndOfRound
      getModifiers hero >>= (`shouldSatisfy` notElem (DefenseModifier (-2)))

    it "exhausts only a character controlled by the defending player" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy one eid
      defender <- strongDefender one
      ready <- heroOf one "01007"
      other <- heroOf two "01007"
      _ <- setEncounterDeck [Enemies.angmarWargRider, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack one eid
      defendAttack one defender
      chooseTarget one ready
      passAttackActions
      isExhausted ready `shouldReturn` True
      isExhausted other `shouldReturn` False

    it "deals a new shadow for the Werewolf's additional attack" . invaderTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      defender <- strongDefender pid
      other <- heroOf pid "01007"
      _ <- setEncounterDeck [Enemies.savageWerewolf, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      defendAttack pid other
      length <$> getEncounterDeck `shouldReturn` 1

    it "prevents its defender from readying for the rest of the round" . invaderTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      defender <- strongDefender pid
      _ <- setEncounterDeck [Enemies.vanguardOfCarnDum, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      run $ Ready defender
      isExhausted defender `shouldReturn` True
      run $ EndEffects UntilEndOfRound
      run $ Ready defender
      isExhausted defender `shouldReturn` False
