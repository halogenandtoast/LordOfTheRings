module LordOfTheRings.Standalone.MassingAtOsgiliathSpec (spec) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Helpers.Attachment (cardHosts, playableCardHosts)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (Blank, WillpowerModifier, CannotPlayAttachmentsOn))
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkAfter)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Types (encounterDiscardL)
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)
import TestImport

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "enemy missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

spec :: Spec
spec = describe "The Massing at Osgiliath encounter cards" do
  it "keeps Uruk Vanguard at its printed attack before the crossing" . gameTest $ \_ -> do
    eid <- stageEnemy Enemies.urukVanguard
    getEnemyAttack eid `shouldReturn` 2
    crossedAnduin `shouldReturn` False

  it "retrieves only the top Scout enemy, skipping other encounter cards" . gameTest $ \_ -> do
    cards <- traverse genEncounterCard [Locations.oldForestRoad, Enemies.goblinScout, Enemies.urukVanguard, Enemies.goblinScout]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
    captain <- stageEnemy Enemies.wainriderCaptain
    run $ WhenRevealed (toTarget captain)
    scouts <- getsGame $ filter ((== toCardCode Enemies.goblinScout) . toCardCode) . toList . (.entities.enemies)
    length scouts `shouldBe` 1
    map ((.id) . toCard) scouts `shouldBe` map (.id) (take 1 $ drop 1 cards)
    getEncounterDiscard `shouldReturn` take 1 cards <> drop 2 cards

  it "handles a discard pile without Scouts without adding a new card" . gameTest $ \_ -> do
    cards <- traverse genEncounterCard [Locations.oldForestRoad, Enemies.urukVanguard]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
    captain <- stageEnemy Enemies.wainriderCaptain
    run $ WhenRevealed (toTarget captain)
    getEncounterDiscard `shouldReturn` cards
    getsGame (length . (.entities.enemies)) `shouldReturn` 1

  it "applies the Witch-king's willpower penalty only while he is in staging" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    beravor <- heroOf pid "01012"
    getModifiers beravor >>= (`shouldSatisfy` elem (WillpowerModifier (-1)))
    getModifiers witch >>= (`shouldSatisfy` elem CannotPlayAttachmentsOn)
    run $ PlaceEnemy witch (EngagedWith pid)
    getModifiers beravor >>= (`shouldSatisfy` notElem (WillpowerModifier (-1)))

  it "lets the defending player keep the Witch-king engaged by raising threat" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    run $ PlaceEnemy witch (EngagedWith pid)
    threat <- (.threat) <$> getPlayer pid
    run $ UseCardAbility pid (EnemySource witch) 1 [mkAfter $ EnemyAttacking witch pid]
    chooseOptionMatching "raise threat" pid \case
      Label l _ -> "$label.theWitchKingOsgiliath.threat" `isPrefixOf` l
      _ -> False
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3
    (.placement) <$> getEnemy witch `shouldReturn` EngagedWith pid

  it "returns the Witch-king to staging when the player declines the threat increase" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    run $ PlaceEnemy witch (EngagedWith pid)
    threat <- (.threat) <$> getPlayer pid
    run $ UseCardAbility pid (EnemySource witch) 1 [mkAfter $ EnemyAttacking witch pid]
    chooseOptionMatching "return to staging" pid \case
      Label l _ -> "$label.theWitchKingOsgiliath.return" `isPrefixOf` l
      _ -> False
    (.placement) <$> getEnemy witch `shouldReturn` StagingArea
    (.threat) <$> getPlayer pid `shouldReturn` threat

  it "raises the defending player's threat by the damage Wainriders actually deals" . gameTest $ \pid -> do
    riders <- stageEnemy Enemies.wainriders
    beravor <- heroOf pid "01012"
    threat <- (.threat) <$> getPlayer pid
    run $ DealDamage (EnemyAttackSource riders) beravor 2
    (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  it "deals two damage to the defending character with the Wolves shadow" . gameTest $ \pid -> do
    wolves <- stageEnemy Enemies.wolvesFromMordor
    cid <- (.id) . toCard <$> getEnemy wolves
    beravor <- heroOf pid "01012"
    run $ ResolveShadowEffect cid wolves pid [beravor]
    getDamage beravor `shouldReturn` 2

  it "shuffles Wolves back into the encounter deck after their attack destroys an ally" . gameTest $ \pid -> do
    wolves <- stageEnemy Enemies.wolvesFromMordor
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage (EnemyAttackSource wolves) (toTarget ally) 2
    run $ UseCardAbility pid (EnemySource wolves) 1 [mkAfter $ EnemyAttacking wolves pid]
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getEncounterDeck >>= (`shouldSatisfy` any ((== toCardCode Enemies.wolvesFromMordor) . toCardCode))

  it "keeps Wolves in play after an attack that destroys no character" . gameTest $ \pid -> do
    wolves <- stageEnemy Enemies.wolvesFromMordor
    run $ UseCardAbility pid (EnemySource wolves) 1 [mkAfter $ EnemyAttacking wolves pid]
    (.placement) <$> getEnemy wolves `shouldReturn` StagingArea

  it "forbids playing Forest Snare on the Witch-king while allowing it to be put into play" . gameTest $ \pid -> do
    witch <- stageEnemy Enemies.theWitchKingOsgiliath
    run $ PlaceEnemy witch (EngagedWith pid)
    playableCardHosts pid (toCardCode Attachments.forestSnare) >>= (`shouldSatisfy` notElem (toTarget witch))
    cardHosts pid (toCardCode Attachments.forestSnare) >>= (`shouldSatisfy` elem (toTarget witch))
    snare <- genPlayerCard Attachments.forestSnare (Just pid)
    run $ PutAttachmentIntoPlay pid snare
    attachments <- getsGame $ toList . (.entities.attachments)
    map (.placement) attachments `shouldBe` [AttachedTo $ toTarget witch]

  it "does not raise threat for damage dealt while Wainriders is blank" . gameTest $ \pid -> do
    riders <- stageEnemy Enemies.wainriders
    beravor <- heroOf pid "01012"
    threat <- (.threat) <$> getPlayer pid
    run $ CreateEffect $ Effect GameSource (toTarget riders) [Blank] UntilEndOfPhase
    run $ DealDamage (EnemyAttackSource riders) beravor 1
    getDamage beravor `shouldReturn` 1
    (.threat) <$> getPlayer pid `shouldReturn` threat

  it "raises the attacked player's threat when another player's character takes the damage" . twoPlayerTest testDeck $ \one two -> do
    riders <- stageEnemy Enemies.wainriders
    run $ PlaceEnemy riders (EngagedWith two)
    defender <- heroOf two "01012"
    beforeOne <- (.threat) <$> getPlayer one
    beforeTwo <- (.threat) <$> getPlayer two
    pushAll [DealDamage (EnemyAttackSource riders) defender 2, CheckWindows [mkAfter $ EnemyAttacking riders one]]
    void mainLoop
    (.threat) <$> getPlayer one `shouldReturn` beforeOne + 2
    (.threat) <$> getPlayer two `shouldReturn` beforeTwo
