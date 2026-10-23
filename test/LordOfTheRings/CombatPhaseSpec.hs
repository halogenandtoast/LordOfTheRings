module LordOfTheRings.CombatPhaseSpec (spec) where

import LordOfTheRings.Enemy (Enemy, EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import TestImport

-- | Engage the Forest Spider from Flies and Spiders setup: 2 attack, +1 from its Forced
-- effect on engaging, 1 defense and 4 hit points.
engageSpider :: PlayerId -> GameT Enemy
engageSpider pid = do
  flipFirstQuest pid
  spider <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
  run $ EngageEnemy pid (toId spider)
  pure spider

spider :: GameT Enemy
spider = getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))

spec :: Spec
spec = describe "Combat phase" do
  it "deals a shadow card to each engaged enemy" . gameTest $ \pid -> do
    _ <- engageSpider pid
    shadow <- setEncounterDeck [Locations.oldForestRoad]
    run DealShadowCards
    (.shadowCards) <$> spider `shouldReturn` shadow

  it "deals the attack minus the defender's defense to the defender" . gameTest $ \pid -> do
    _ <- engageSpider pid
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ BeginPhase CombatPhase
    passActions pid
    eowyn <- getHeroWithCode "01007"
    chooseTarget pid eowyn
    passActions pid
    eowyn' <- getHeroWithCode "01007"
    eowyn'.damage `shouldBe` 2
    heroExhausted (toAttrs eowyn') `shouldBe` True

  it "deals the full attack to a hero when undefended" . gameTest $ \pid -> do
    _ <- engageSpider pid
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ BeginPhase CombatPhase
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    beravor <- getHeroWithCode "01012"
    chooseTarget pid beravor
    (.damage) <$> getHeroWithCode "01012" `shouldReturn` 3

  it "discards shadow cards at the end of combat" . gameTest $ \pid -> do
    _ <- engageSpider pid
    shadow <- setEncounterDeck [Locations.oldForestRoad]
    run DealShadowCards
    run $ EndPhase CombatPhase
    (.shadowCards) <$> spider `shouldReturn` []
    getEncounterDiscard `shouldReturn` shadow

  it "deals the attackers' total attack minus the enemy's defense" . gameTest $ \pid -> do
    _ <- engageSpider pid
    run $ PlayerAttacks pid
    -- Beravor (2) and Theodred (2) against the spider's 1 defense
    getHeroWithCode "01012" >>= chooseTarget pid
    getHeroWithCode "01002" >>= chooseTarget pid
    chooseLabel "$label.doneAttacking" pid
    passActions pid
    (.damage) <$> spider `shouldReturn` 3
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "destroys an enemy that takes enough damage" . gameTest $ \pid -> do
    _ <- engageSpider pid
    run $ PlayerAttacks pid
    for_ ["01012", "01002", "01007"] $ getHeroWithCode >=> chooseTarget pid
    passActions pid
    getsGame (length . (.entities.enemies)) `shouldReturn` 0

  it "names the attacking enemy as the source of the defender question" . gameTest $ \pid -> do
    s <- engageSpider pid
    run $ EnemyAttack pid (toId s)
    passActions pid
    Just (QuestionLabel _ source _ _) <- getQuestion pid
    source `shouldBe` Just (toTarget s)

  it "names the attacked enemy as the target when declaring attackers" . gameTest $ \pid -> do
    s <- engageSpider pid
    run $ DeclareAttackers pid (toId s)
    Just (QuestionLabel _ _ targets _) <- getQuestion pid
    targets `shouldBe` [toTarget s]

  it "lets the player choose which engaged enemy attacks next" . gameTest $ \pid -> do
    _ <- engageSpider pid
    _ <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad, Locations.oldForestRoad]
    run RevealEncounterCard
    second <- getsGame (fromJustNote "no second spider" . headMay . map toId . filter ((== StagingArea) . (.placement)) . toList . (.entities.enemies))
    run $ EngageEnemy pid second
    run $ EnemyAttacks pid
    Just (QuestionLabel prompt _ targets _) <- getQuestion pid
    prompt `shouldBe` "$label.chooseNextAttack"
    length targets `shouldBe` 2

  it "turns each shadow card face up as its attack resolves, blank ones too" . gameTest $ \pid -> do
    eid <- toId <$> engageSpider pid
    shadow <- setEncounterDeck [Locations.oldForestRoad]
    run DealShadowCards
    run $ ResolveShadow eid pid Nothing
    engaged <- getsGame (fromJustNote "gone" . lookup eid . mapToList . (.entities.enemies))
    enemyShadowRevealed (toAttrs engaged) `shouldBe` map (.id) shadow
