module LordOfTheRings.CombatPhaseSpec (spec) where

import LordOfTheRings.Enemy (Enemy)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import TestImport

-- | Engage the Forest Spider from Flies and Spiders setup (2 attack, 1 defense, 4 hit points).
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
    eowyn <- getHeroWithCode "01007"
    chooseTarget pid eowyn
    eowyn' <- getHeroWithCode "01007"
    eowyn'.damage `shouldBe` 1
    heroExhausted (toAttrs eowyn') `shouldBe` True

  it "deals the full attack to a hero when undefended" . gameTest $ \pid -> do
    _ <- engageSpider pid
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ BeginPhase CombatPhase
    chooseLabel "Take the attack undefended" pid
    beravor <- getHeroWithCode "01012"
    chooseTarget pid beravor
    (.damage) <$> getHeroWithCode "01012" `shouldReturn` 2

  it "discards shadow cards at the end of combat" . gameTest $ \pid -> do
    _ <- engageSpider pid
    shadow <- setEncounterDeck [Locations.oldForestRoad]
    run DealShadowCards
    run $ EndPhase CombatPhase
    (.shadowCards) <$> spider `shouldReturn` []
    getEncounterDiscard `shouldReturn` shadow
