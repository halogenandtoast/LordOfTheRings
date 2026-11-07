module LordOfTheRings.Standalone.PrancingPonyScenarioSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardType (EnemyType, LocationType), cdCardType, toCardDef)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.I18n (cardVar, ikeyWith)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (PlacingQuestingProgress))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (murderAtThePrancingPony)
import LordOfTheRings.Scenario.Types (scenarioMode, GameMode (..))
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers (Meta (..), getMeta, suspects)
import LordOfTheRings.Standalone.PrancingPonyInvestigationSpec (meta)
import TestImport

finishQuestions :: Int -> GameT ()
finishQuestions n | n <= 0 = error "Prancing Pony questions did not finish"
finishQuestions n = do
  asked <- getsGame (headMay . Map.keys . (.question))
  for_ asked $ \pid -> do
    chooseOptionMatching "finish setup or encounter search" pid (const True)
    finishQuestions (n - 1)

reload :: GameT ()
reload = do
  current <- getGame
  case eitherDecode (encode current) of
    Left err -> liftIO $ expectationFailure err
    Right restored -> putGame =<< preloadModifiers restored

spec :: Spec
spec = describe "Prancing Pony complete scenario" do
  for_ [NormalMode, EasyMode] $ \mode -> for_ [1 .. 4] $ \size ->
    it ("sets up and wins with the original physical supply, " <> show size <> " players, " <> show mode) do
      seats <- for [1 .. size] $ \seat -> either (error . show) id <$> loadPlayer ("Player " <> tshow seat) testDeck
      let scenario = overAttrs (\a -> a {scenarioMode = mode}) murderAtThePrancingPony
      env <- newGameEnv (newTableIn scenario $ fromJustNote "empty table" $ nonEmpty seats) (const $ pure ()) 1
      runGameT env do
        run Setup
        finishQuestions 100
        lead <- getLead
        toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.investigatingTheCrime
        (.flipped) <$> getActiveQuest `shouldReturn` True
        stored <- meta
        length stored.hidden `shouldBe` 2
        length stored.investigationDeck `shouldBe` if mode == EasyMode then 9 else 10
        deck <- getEncounterDeck
        length deck `shouldBe` if mode == EasyMode then 27 else 36
        locations <- getsGame (toList . (.entities.locations))
        let supply = map (.id) (deck <> stored.hidden <> stored.investigationDeck) <> map ((.cardId) . toAttrs) locations
        length supply `shouldBe` if mode == EasyMode then 40 else 50
        length (nub supply) `shouldBe` length supply
        players <- getsGame (toList . (.players))
        map (length . (.hand)) players `shouldBe` replicate size 6
        heroes <- getsGame (toList . (.entities.heroes))
        map (.resources) heroes `shouldBe` replicate (3 * size) (if mode == EasyMode then 1 else 0)
        pony <- getsGame (toId . fromJustNote "missing Prancing Pony" . find ((== toCardCode Locations.thePrancingPonyMurder) . toCardCode) . toList . (.entities.locations))
        run $ PlaceTokens GameSource (LocationTarget pony) Progress 2
        chooseLabel "$label.prancingPony.drawTwoEach" lead
        firstStage <- toTarget <$> getActiveQuest
        replicateM_ 3 do
          run $ EndEffects UntilEndOfRound
          run $ PlaceTokens GameSource firstStage Progress 50
        resolveRevealed
        toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.runningOutOfTime
        secondStage <- toTarget <$> getActiveQuest
        run $ CreateEffect $ Effect GameSource GameTarget [PlacingQuestingProgress] UntilEndOfPhase
        run $ PlaceTokens GameSource secondStage Progress 1
        chooseLabel "$label.prancingPony.accuse" lead
        solution <- (.hidden) <$> meta
        let culprit = fromJustNote "missing suspect" $ find ((== EnemyType) . cdCardType . toCardDef) solution
            shelter = fromJustNote "missing hideout" $ find ((== LocationType) . cdCardType . toCardDef) solution
        chooseLabel (ikeyWith "label.prancingPony.nameCard" $ cardVar "card" culprit) lead
        reload
        chooseLabel (ikeyWith "label.prancingPony.nameCard" $ cardVar "card" shelter) lead
        resolveRevealed
        finishQuestions 100
        toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.apprehendingTheKiller
        (.accusationCorrect) <$> meta `shouldReturn` Just True
        reload
        -- Explore the actual hideout and defeat every actual enemy. The physical
        -- solution and stage-3 searches stay intact; no substitute encounters enter.
        -- Dark Alley also protects enemies in staging, so clear the other
        -- locations before dealing damage there.
        locationIds <- getsGame (map toId . toList . (.entities.locations))
        for_ locationIds $ \lid -> run $ PlaceTokens GameSource (LocationTarget lid) Progress 99
        brigands <- getsGame (map toId . filter ((`notElem` map toCardCode suspects) . toCardCode) . toList . (.entities.enemies))
        for_ brigands $ \eid -> run $ DealDamage GameSource (EnemyTarget eid) 99
        suspectIds <- getsGame (map toId . filter ((`elem` map toCardCode suspects) . toCardCode) . toList . (.entities.enemies))
        for_ suspectIds $ \eid -> replicateM_ 2 do
          run $ EndEffects UntilEndOfRound
          run $ DealDamage GameSource (EnemyTarget eid) 99
        getsGame (.outcome) `shouldReturn` Just Won
        getsGame (length . (.hidden) . getMeta . toAttrs . (.scenario)) `shouldReturn` 0
