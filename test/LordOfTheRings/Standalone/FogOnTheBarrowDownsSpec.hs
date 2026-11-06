module LordOfTheRings.Standalone.FogOnTheBarrowDownsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Matcher (getHero, getThreat)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkAfter)
import TestImport

wights :: [CardDef]
wights = [Enemies.barrowWight, Enemies.coldWight, Enemies.darkWight]

stageWight :: CardDef -> GameT EnemyId
stageWight def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "missing Wight" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies)

restriction :: PlayerId -> CardDef -> GameT (Target, ModifierType)
restriction pid def
  | toCardCode def == toCardCode Enemies.barrowWight = pure (PlayerTarget pid, CannotReduceThreat)
  | toCardCode def == toCardCode Enemies.coldWight = pure (PlayerTarget pid, CannotDrawCardsFromCardEffects)
  | otherwise = (,CannotGainResourcesFromCardEffects) <$> heroOf pid "01012"

passAttackActions :: GameT ()
passAttackActions = do
  questions <- getsGame (Map.toList . (.question))
  for_ (headMay [pid | (pid, QuestionLabel "$label.takeAction" _ _ _) <- questions]) \pid -> do
    passActions pid
    passAttackActions

defendAttack :: PlayerId -> Target -> GameT ()
defendAttack pid target = passAttackActions >> chooseTarget pid target >> passAttackActions

prepareVictimController :: PlayerId -> GameT ()
prepareVictimController pid = do
  run $ RaiseThreat pid 10
  _ <- putCardIntoHand pid Allies.guardOfTheCitadel
  for_ ["01012", "01007", "01002"] \code -> do
    h <- heroOf pid code
    run $ PlaceTokens GameSource h Resource 3

resourcesOf :: PlayerId -> GameT [Int]
resourcesOf pid = for ["01012", "01007", "01002"] \code -> do
  target <- heroOf pid code
  case target of
    HeroTarget hid -> (.resources) . toAttrs <$> getHero hid
    _ -> error "not a hero"

checkVictimEffect :: CardDef -> PlayerId -> GameT ()
checkVictimEffect def pid = do
  getThreat pid `shouldReturn` if toCardCode def == toCardCode Enemies.barrowWight then 12 else 10
  length . (.hand) <$> getPlayer pid `shouldReturn` if toCardCode def == toCardCode Enemies.coldWight then 0 else 1
  resourcesOf pid `shouldReturn` if toCardCode def == toCardCode Enemies.darkWight then [0, 0, 0] else [3, 3, 3]

checkUnaffected :: PlayerId -> GameT ()
checkUnaffected pid = do
  getThreat pid `shouldReturn` 10
  length . (.hand) <$> getPlayer pid `shouldReturn` 1
  resourcesOf pid `shouldReturn` [3, 3, 3]

spec :: Spec
spec = describe "Fog on the Barrow-downs Wights" do
  for_ wights \def -> describe (show $ toCardCode def) do
    it "applies its restriction only to its engaged player, immediately follows movement and respects blanking" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageWight def
      (t1, prohibited) <- restriction one def
      (t2, _) <- restriction two def
      getModifiers t1 >>= (`shouldSatisfy` notElem prohibited)
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)
      run $ EngageEnemy one eid
      getModifiers t1 >>= (`shouldSatisfy` elem prohibited)
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)
      run $ EngageEnemy two eid
      getModifiers t1 >>= (`shouldSatisfy` notElem prohibited)
      getModifiers t2 >>= (`shouldSatisfy` elem prohibited)
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfPhase
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)
      run $ EndEffects UntilEndOfPhase
      getModifiers t2 >>= (`shouldSatisfy` elem prohibited)
      run $ PlaceEnemy eid StagingArea
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)

    it "does not punish a player when its attack destroys no character" . gameTest $ \pid -> do
      prepareVictimController pid
      eid <- stageWight def
      run $ EngageEnemy pid eid
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [DefenseModifier 30] UntilEndOfRound
      run $ EnemyAttack pid eid
      defendAttack pid h
      checkUnaffected pid

    it "punishes the controller of an ally its attack destroys" . gameTest $ \pid -> do
      prepareVictimController pid
      eid <- stageWight def
      run $ EngageEnemy pid eid
      ally <- putAllyIntoPlay pid Allies.snowbournScout
      run $ EnemyAttack pid eid
      defendAttack pid ally
      checkVictimEffect def pid

    it "retains the controller of a destroyed hero after that hero leaves play" . gameTest $ \pid -> do
      prepareVictimController pid
      eid <- stageWight def
      run $ EngageEnemy pid eid
      h <- heroOf pid "01002"
      run $ PlaceTokens GameSource h Damage 3
      run $ EnemyAttack pid eid
      defendAttack pid h
      getsGame (length . (.entities.heroes)) `shouldReturn` 2
      getThreat pid `shouldReturn` if toCardCode def == toCardCode Enemies.barrowWight then 12 else 10
      length . (.hand) <$> getPlayer pid `shouldReturn` if toCardCode def == toCardCode Enemies.coldWight then 0 else 1
      remaining <- getsGame (map ((.resources) . toAttrs) . toList . (.entities.heroes))
      remaining `shouldBe` if toCardCode def == toCardCode Enemies.darkWight then [0, 0] else [3, 3]

    it "punishes a killed Sentinel's controller rather than the defending player" . twoPlayerTest testDeck $ \one two -> do
      for_ [one, two] prepareVictimController
      eid <- stageWight def
      run $ EngageEnemy one eid
      ally <- putAllyIntoPlay two Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource ally [GainKeyword Sentinel] UntilEndOfRound
      run $ EnemyAttack one eid
      passAttackActions
      chooseLabel "$label.takeAttackUndefended" one
      chooseTarget two ally
      passAttackActions
      checkUnaffected one
      checkVictimEffect def two

  it "Barrow-wight blocks Gandalf's threat reduction until it leaves play" . gameTest $ \pid -> do
    run $ RaiseThreat pid 10
    eid <- stageWight Enemies.barrowWight
    run $ EngageEnemy pid eid
    gandalf <- putAllyIntoPlay pid Allies.gandalf
    chooseAbilityOf pid gandalf
    chooseLabel "$label.gandalf.threat" pid
    getThreat pid `shouldReturn` 10
    run $ Discard GameSource (EnemyTarget eid)
    run $ UseCardAbility pid (case gandalf of AllyTarget aid -> AllySource aid; _ -> error "not an ally") 1 []
    chooseLabel "$label.gandalf.threat" pid
    getThreat pid `shouldReturn` 5

  it "Cold-wight blocks Gandalf's card-effect draw while ordinary resource-phase drawing still works" . gameTest $ \pid -> do
    eid <- stageWight Enemies.coldWight
    run $ EngageEnemy pid eid
    gandalf <- putAllyIntoPlay pid Allies.gandalf
    chooseAbilityOf pid gandalf
    chooseLabel "$label.gandalf.draw" pid
    length . (.hand) <$> getPlayer pid `shouldReturn` 0
    run $ DoPhase ResourcePhase
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "Dark-wight blocks resources from player and encounter card effects, while framework resources remain legal" . gameTest $ \pid -> do
    eid <- stageWight Enemies.darkWight
    run $ EngageEnemy pid eid
    h <- heroOf pid "01012"
    for_ [EnemySource eid, case h of HeroTarget hid -> HeroSource hid; _ -> error "not a hero"] \source ->
      run $ PlaceTokens source h Resource 2
    resourcesOf pid `shouldReturn` [0, 0, 0]
    run $ PlaceTokens GameSource h Resource 1
    resourcesOf pid `shouldReturn` [1, 0, 0]
    run $ Discard GameSource (EnemyTarget eid)
    run $ PlaceTokens (EnemySource eid) h Resource 2
    resourcesOf pid `shouldReturn` [3, 0, 0]

  it "Dark-wight neither bars nor discards resources on an ally's own resource pool" . gameTest $ \pid -> do
    prepareVictimController pid
    eid <- stageWight Enemies.darkWight
    run $ EngageEnemy pid eid
    treebeard <- putAllyIntoPlay pid Allies.treebeard
    run $ PlaceTokens (EnemySource eid) treebeard Resource 2
    ally <- putAllyIntoPlay pid Allies.snowbournScout
    run $ EnemyAttack pid eid
    defendAttack pid ally
    resourcesOf pid `shouldReturn` [0, 0, 0]
    getsGame ((.resources) . toAttrs . fromJustNote "Treebeard missing" . find ((== treebeard) . toTarget) . toList . (.entities.allies)) `shouldReturn` 2

  it "does not count a character killed by a separate shadow effect as an attack kill" . gameTest $ \pid -> do
    prepareVictimController pid
    eid <- stageWight Enemies.barrowWight
    ally <- putAllyIntoPlay pid Allies.snowbournScout
    run $ DealDamage (EnemySource eid) ally 1
    run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    checkUnaffected pid

  for_ [Enemies.barrowWight, Enemies.coldWight] \def ->
    it ("punishes each character's controller exactly once for a multi-character attack kill: " <> show (toCardCode def)) . gameTest $ \pid -> do
      prepareVictimController pid
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      eid <- stageWight def
      a <- putAllyIntoPlay pid Allies.snowbournScout
      b <- putAllyIntoPlay pid Allies.snowbournScout
      for_ [a, b] \target -> run $ DealDamage (EnemyAttackSource eid) target 1
      run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
      getThreat pid `shouldReturn` if toCardCode def == toCardCode Enemies.barrowWight then 14 else 10
      length . (.hand) <$> getPlayer pid `shouldReturn` if toCardCode def == toCardCode Enemies.coldWight then 1 else 3
