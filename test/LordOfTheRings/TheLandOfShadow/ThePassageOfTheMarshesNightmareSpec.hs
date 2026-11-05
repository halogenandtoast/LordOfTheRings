module LordOfTheRings.TheLandOfShadow.ThePassageOfTheMarshesNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationCardId, locationTokens))
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Modifier (ModifierType (Blank, CannotReduceThreat, CannotHaveAttachments, CannotTakeDamage, HitPointModifier, CancelNextDamage, DamageReducedTo, DamageRedirectedTo))
import LordOfTheRings.Scenario (thePassageOfTheMarshes)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkWhen)
import TestImport

marshes :: (PlayerId -> GameT ()) -> IO ()
marshes = gameTestIn thePassageOfTheMarshes testDeck

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

activeLocation :: GameT LocationId
activeLocation = activeLocationWith Locations.stagnantPools

activeLocationWith :: CardDef -> GameT LocationId
activeLocationWith def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  lid <- getsGame (toId . fromJustNote "active location" . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))
  run $ PlaceLocation lid ActiveLocation
  pure lid

mireTokens :: LocationId -> GameT Int
mireTokens lid = countTokens Resource . locationTokens . toAttrs <$> getLocation lid

attack :: PlayerId -> EnemyId -> Target -> GameT ()
attack pid eid defender = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseTarget pid defender
  passActions pid
  resolveShadowsInOrder pid
  passActions pid

spec :: Spec
spec = describe "The Passage of the Marshes Nightmare" do
  it "Rotting Thing counts redirected attack damage against the character that actually takes it" . marshes $ \pid -> do
    lid <- activeLocationWith Locations.theOldFord
    eid <- stage Enemies.rottingThing
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    recipient <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    run $ CreateEffect $ Effect GameSource defender [DamageRedirectedTo recipient] UntilEndOfRound
    attack pid eid defender
    mireTokens lid `shouldReturn` 5
    getsGame (any ((== defender) . toTarget) . toList . (.entities.allies)) `shouldReturn` True
    getsGame (any ((== recipient) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Rotting Thing counts only excess damage actually dealt after partial prevention" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [CancelNextDamage 1] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 3

  it "Rotting Thing adds no mire when damage is reduced to exactly the defender's hit points" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [DamageReducedTo 2] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  for_ [("Pale Faces", Enemies.paleFaces), ("Rotting Thing", Enemies.rottingThing)] \(title, def) -> do
    it (title <> " prevents only its engaged player from reducing threat") . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
      for_ [one, two] (\pid -> run $ RaiseThreat pid 10)
      eid <- stage def
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem CannotReduceThreat)
      run $ EngageEnemy one eid
      threatsBefore <- traverse (fmap (.threat) . getPlayer) [one, two]
      runQueueT $ Lifted.lowerThreatBy GameSource one 3
      runQueueT $ Lifted.lowerThreatBy GameSource two 3
      void mainLoop
      traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` zipWith (-) threatsBefore [0, 3]
      run $ PlaceEnemy eid StagingArea
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem CannotReduceThreat)

    it ("blanking " <> title <> " removes its threat restriction") . marshes $ \pid -> do
      eid <- stage def
      run $ EngageEnemy pid eid
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem CannotReduceThreat)

  it "Pale Faces raises the attacked player's threat by 4 on every attack" . marshes $ \pid -> do
    eid <- stage Enemies.paleFaces
    threatBefore <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
    run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 8)

  it "Pale Faces targets the attacked player instead of its engaged player" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    eid <- stage Enemies.paleFaces
    run $ EngageEnemy one eid
    threatsBefore <- traverse (fmap (.threat) . getPlayer) [one, two]
    run $ CheckWindows [mkWhen $ EnemyAttacking eid two]
    traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` zipWith (+) threatsBefore [0, 4]

  it "blanked Pale Faces does not raise threat when attacking" . marshes $ \pid -> do
    eid <- stage Enemies.paleFaces
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    threatBefore <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore

  it "Pale Faces' shadow uses the active location's Mire value, not its mire tokens" . marshes $ \pid -> do
    lid <- activeLocation
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 3
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.paleFaces
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 7
    run $ EndEffects UntilEndOfAttack
    getEnemyAttack eid `shouldReturn` 2

  it "Pale Faces' shadow adds no attack without an active location" . marshes $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.paleFaces
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 2

  it "Rotting Thing places one mire token for each point of excess attack damage" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    attack pid eid ally
    mireTokens lid `shouldReturn` 4
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Rotting Thing counts a wounded defender's remaining hit points" . marshes $ \pid -> do
    lid <- activeLocationWith Locations.theOldFord
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource ally 1
    attack pid eid ally
    mireTokens lid `shouldReturn` 5

  it "Rotting Thing can trigger the active location's Mire discard and Forced effect" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource ally 1
    threatBefore <- (.threat) <$> getPlayer pid
    attack pid eid ally
    select (LocationWithId lid) `shouldReturn` []
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.stagnantPools))
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 5)

  it "Rotting Thing adds no mire for an exact lethal attack using modified hit points" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [HitPointModifier 4] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Rotting Thing adds no mire when its damage cannot destroy the defender" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [CannotTakeDamage] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` True

  it "blanked Rotting Thing does not place mire after destroying a defender" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0

  it "Rotting Thing cannot receive attachments until its text is blanked" . marshes $ \pid -> do
    eid <- stage Enemies.rottingThing
    run $ EngageEnemy pid eid
    getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 3
    card <- putCardIntoHand pid Attachments.forestSnare
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player card `shouldReturn` False
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    getModifiers eid >>= (`shouldSatisfy` notElem CannotHaveAttachments)
    canPlay player card `shouldReturn` True
