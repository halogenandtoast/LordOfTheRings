module LordOfTheRings.DamageSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Modifier (ModifierType (ExhaustsCharactersDamagedByExcess))
import TestImport

spec :: Spec
spec = describe "Damage" do
  it "exhausts each character excess damage goes to, as Terrible Strength reads" . gameTest $ \pid -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    run RevealEncounterCard
    spider <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
    run $ CreateEffect $ Effect GameSource (EnemyTarget spider) [ExhaustsCharactersDamagedByExcess] UntilEndOfPhase
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ DamageAnotherCharacter (EnemyAttackSource spider) pid beravor
    chooseTarget pid eowyn
    getCharacterDamage eowyn `shouldReturn` 1
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True

  it "places damage on a character" . gameTest $ \_ -> do
    beravor <- getHeroWithCode "01012"
    run $ DealDamage GameSource (toTarget beravor) 2
    (.damage) <$> getHeroWithCode "01012" `shouldReturn` 2

  it "destroys a hero whose damage reaches its hit points" . gameTest $ \pid -> do
    beravor <- getHeroWithCode "01012"
    run $ DealDamage GameSource (toTarget beravor) 4
    getsGame (Map.member (toId beravor) . (.entities.heroes)) `shouldReturn` False
    map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` ["01012"]

  it "discards a destroyed enemy" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    run RevealEncounterCard
    spider <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
    run $ DealDamage GameSource (toTarget spider) 4
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` ["01096"]

  it "puts a destroyed enemy with victory points in the victory display" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.hummerhorns]
    run RevealEncounterCard
    hummerhorns <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
    run $ DealDamage GameSource (toTarget hummerhorns) 3
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getEncounterDiscard `shouldReturn` []

  it "lets later damage in a batch pass over a character already destroyed" . gameTest $ \_ -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ Run [DealDamage GameSource eowyn 3, DealDamage GameSource eowyn 1]
    getsGame (length . (.entities.heroes)) `shouldReturn` 2

  it "treats an attack as undefended when its defender left play before the damage" . gameTest $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    _ <- setEncounterDeck [Enemies.hummerhorns]
    run RevealEncounterCard
    eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
    run $ EngageEnemy pid eid
    chooseOptionMatching "take the Hummerhorns damage" pid (const True)
    run $ Discard GameSource eowyn
    run $ AttackDamage eid eowyn
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldBe` "$label.undefendedDamage"

  it "assigns all the archery damage before dealing it, none past a character's hit points" . gameTest $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ ArcheryDamage 4 []
    replicateM_ 3 (chooseTarget pid eowyn)
    -- assigned, not yet dealt, and Éowyn has no hit points left to take more
    getDamage eowyn `shouldReturn` 0
    targets <- maybe [] (\q -> [t | TargetLabel t _ <- questionChoices q]) <$> getQuestion pid
    targets `shouldSatisfy` \ts -> notNull ts && eowyn `notElem` ts
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1
    getsGame (length . (.entities.heroes)) `shouldReturn` 2

  it "assigns damage divided among characters no further than a character's hit points" . gameTest $ \pid -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ DealDamageDivided GameSource pid 4 []
    replicateM_ 3 (chooseTarget pid eowyn)
    targets <- maybe [] (\q -> [t | TargetLabel t _ <- questionChoices q]) <$> getQuestion pid
    targets `shouldSatisfy` \ts -> notNull ts && eowyn `notElem` ts
