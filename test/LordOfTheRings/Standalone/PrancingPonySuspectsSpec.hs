module LordOfTheRings.Standalone.PrancingPonySuspectsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Matcher (getEnemy, getThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Standalone.PrancingPonySpec hiding (spec)
import TestImport hiding (gameTest, twoPlayerTest)

suspects :: [CardDef]
suspects = [Enemies.toddTheTroll, Enemies.susieTheSpider, Enemies.wolfPeltJake, Enemies.johnnyGoblinFingers, Enemies.oldOrcEyes]

reload :: GameT ()
reload = do
  current <- getGame
  case eitherDecode (encode current) of
    Left err -> liftIO $ expectationFailure err
    Right restored -> putGame =<< preloadModifiers restored

spec :: Spec
spec = describe "Prancing Pony suspects" do
  for_ suspects $ \def ->
    it ("forbids attachments only while unblanked: " <> show (toCardCode def)) . gameTest $ \_ -> do
      eid <- stageEnemy def
      getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      getModifiers eid >>= (`shouldSatisfy` notElem CannotHaveAttachments)

  describe "Todd the Troll" do
    it "adds no excess threat when Frodo cancels the attack's damage" . gameTestWith (deckList [toCardCode Heroes.frodoBaggins, "01007", "01002"] testDeck.cards) $ \pid -> do
      run $ DoStep 2 Setup
      initial <- getThreat pid
      eid <- stageEnemy Enemies.toddTheTroll
      run $ EngageEnemy pid eid
      defender <- heroOf pid (toCardCode Heroes.frodoBaggins)
      run $ EnemyAttack pid eid
      passAttackActions
      chooseTarget pid defender
      chooseAbilityOf pid defender
      passAttackActions
      getDamage defender `shouldReturn` 0
      getThreat pid `shouldReturn` initial + 5

    for_ [3, 10] $ \prevented ->
      it ("counts damage after prevention, prevented: " <> show prevented) . gameTest $ \pid -> do
        eid <- stageEnemy Enemies.toddTheTroll
        run $ EngageEnemy pid eid
        defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
        run $ CreateEffect $ Effect GameSource defender [CancelNextDamage prevented] UntilEndOfRound
        run $ EnemyAttack pid eid
        defendAttack pid defender
        getThreat pid `shouldReturn` 27 + max 0 (5 - prevented)

    it "charges the attacked player when another player's Sentinel takes excess damage" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.toddTheTroll
      run $ EngageEnemy one eid
      defender <- putAllyIntoPlay two Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource defender [GainKeyword Sentinel] UntilEndOfRound
      run $ EnemyAttack one eid
      passAttackActions
      chooseLabel "$label.takeAttackUndefended" one
      chooseTarget two defender
      passAttackActions
      getThreat one `shouldReturn` 32
      getThreat two `shouldReturn` 27

    for_ [0, 1] $ \alreadyDamaged ->
      it ("converts only excess combat damage to threat with prior damage " <> show alreadyDamaged) . gameTest $ \pid -> do
        eid <- stageEnemy Enemies.toddTheTroll
        run $ EngageEnemy pid eid
        defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
        when (alreadyDamaged > 0) $ run $ DealDamage GameSource defender alreadyDamaged
        run $ EnemyAttack pid eid
        defendAttack pid defender
        getThreat pid `shouldReturn` 32 + alreadyDamaged

    it "does not convert noncombat damage or a fully defended attack" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.toddTheTroll
      defender <- strongDefender pid
      run $ EngageEnemy pid eid
      run $ DealDamage GameSource defender 1
      run $ EnemyAttack pid eid
      defendAttack pid defender
      getThreat pid `shouldReturn` 27

  describe "Susie the Spider" do
    it "counts herself and other Brigands, and loses immunity when the last other Brigand leaves" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.susieTheSpider
      _ <- stageEnemy Enemies.forestSpider
      getEnemyThreat eid `shouldReturn` 1
      getModifiers eid >>= (`shouldSatisfy` notElem CannotTakeDamage)
      brigand <- stageEnemy Enemies.greenwayFootpad
      getEnemyThreat eid `shouldReturn` 2
      run $ DealDamage GameSource (EnemyTarget eid) 2
      getDamage (EnemyTarget eid) `shouldReturn` 0
      run $ Destroy GameSource (EnemyTarget brigand)
      getEnemyThreat eid `shouldReturn` 1
      run $ DealDamage GameSource (EnemyTarget eid) 2
      getDamage (EnemyTarget eid) `shouldReturn` 2

    it "counts engaged Brigands as well as those in staging, and blanks both abilities" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.susieTheSpider
      other <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy pid other
      getEnemyThreat eid `shouldReturn` 2
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      getEnemyThreat eid `shouldReturn` 0
      getModifiers eid >>= (`shouldSatisfy` notElem CannotTakeDamage)

  describe "Wolf-pelt Jake" do
    it "returns to staging after his attack destroys an ally" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.wolfPeltJake
      run $ EngageEnemy pid eid
      defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ EnemyAttack pid eid
      defendAttack pid defender
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "stays engaged after destroying a hero" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.wolfPeltJake
      run $ EngageEnemy pid eid
      defender <- heroOf pid "01012"
      run $ DealDamage GameSource defender 1
      run $ EnemyAttack pid eid
      defendAttack pid defender
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith pid

    it "stays engaged when the defender survives" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.wolfPeltJake
      run $ EngageEnemy pid eid
      defender <- strongDefender pid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith pid

  describe "Johnny Goblin-fingers" do
    for_ [(Allies.gandalf, 5), (Events.standAndFight, 0)] $ \(def, cost) ->
      it ("uses the actual discarded card's printed cost: " <> show cost) . twoPlayerTest testDeck $ \one two -> do
        eid <- stageEnemy Enemies.johnnyGoblinFingers
        run $ EngageEnemy one eid
        card <- putCardIntoHand one def
        _ <- putCardIntoHand two Allies.gandalf
        defender <- strongDefender one
        run $ EnemyAttack one eid
        defendAttack one defender
        map (.id) . (.discard) <$> getPlayer one `shouldReturn` [card.id]
        getThreat one `shouldReturn` 27 + cost
        getThreat two `shouldReturn` 27
        length . (.hand) <$> getPlayer two `shouldReturn` 1

    for_ [False, True] $ \blocked ->
      it ("raises no threat without an actual discard, discard prohibited: " <> show blocked) . gameTest $ \pid -> do
        eid <- stageEnemy Enemies.johnnyGoblinFingers
        run $ EngageEnemy pid eid
        when blocked do
          _ <- putCardIntoHand pid Allies.gandalf
          run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
        defender <- strongDefender pid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        getThreat pid `shouldReturn` 27
        length . (.hand) <$> getPlayer pid `shouldReturn` if blocked then 1 else 0

  describe "Old Orc-eyes" do
    for_ [0, 1, 2] $ \blanks ->
      it ("makes exactly one additional shadowless attack for blank shadows: " <> show blanks) . gameTest $ \pid -> do
        eid <- stageEnemy Enemies.oldOrcEyes
        run $ EngageEnemy pid eid
        defender <- strongDefender pid
        another <- heroOf pid "01007"
        run $ CreateEffect $ Effect GameSource another [DefenseModifier 10] UntilEndOfRound
        _ <- setEncounterDeck $ replicate (blanks + 1) Locations.oldForestRoad
        replicateM_ blanks $ run $ DealShadowCard eid
        reload
        run $ EnemyAttack pid eid
        defendAttack pid defender
        resolveShadowsInOrder pid
        passAttackActions
        when (blanks > 0) $ defendAttack pid another
        getQuestion pid `shouldReturn` Nothing
        length <$> getEncounterDeck `shouldReturn` 1
        when (blanks > 0) $ (.shadowCards) . toAttrs <$> getEnemy eid `shouldReturn` []

    it "does not trigger from a shadow with an effect" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldOrcEyes
      run $ EngageEnemy pid eid
      defender <- strongDefender pid
      _ <- setEncounterDeck [Enemies.greenwayFootpad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 1

    it "checks attack restrictions again before the additional attack" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldOrcEyes
      run $ EngageEnemy pid eid
      defender <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource defender [DefenseModifier 10] UntilEndOfRound
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      passAttackActions
      chooseTarget pid defender
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
      passAttackActions
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 1

    it "keeps its scheduled attack if its text is blanked after the shadow turns faceup" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldOrcEyes
      run $ EngageEnemy pid eid
      defender <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource defender [DefenseModifier 10] UntilEndOfRound
      another <- strongDefender pid
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      passAttackActions
      chooseTarget pid defender
      chooseLabel "$label.pass" pid
      getModifiers eid >>= (`shouldSatisfy` elem (UsedThisAttack $ toCardCode Enemies.oldOrcEyes))
      reload
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      passAttackActions
      defendAttack pid another
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 1

    it "ignores blank shadows discarded before they turn faceup" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldOrcEyes
      run $ EngageEnemy pid eid
      defender <- strongDefender pid
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      run $ DiscardShadowCards eid
      defendAttack pid defender
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 1
