module LordOfTheRings.Standalone.DolGuldurSetupSpec (spec) where

import LordOfTheRings.Card (CardType (QuestType, SetupType), EncounterCard (..), IsCard (toCard), cdCardType, toCardDef)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyShadowCards)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur)
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers (cityStrength)
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioQuestDeck, scenarioSetAside)
import TestImport

finishSetupChoices :: Int -> GameT ()
finishSetupChoices remaining = do
  questions <- getsGame (.question)
  case headMay (mapToList questions) of
    Nothing -> pure ()
    Just (pid, _) | remaining > 0 -> do
      chooseOptionMatching "complete native Dol Guldur setup" pid (const True)
      finishSetupChoices $ remaining - 1
    _ -> error "Dol Guldur setup did not settle"

nativeSetupTable :: Int -> Maybe Int -> GameT () -> IO ()
nativeSetupTable size ordinal body = do
  -- Use four disjoint core hero teams, preserving the uniqueness rule locally.
  seats <- for (zip [1 :: Int ..] $ take size starterDecks) $ \(seat, (_, deck)) ->
    either (error . show) id <$> loadPlayer ("Attacker " <> tshow seat) deck
  env <- newGameEnv (newTableIn attackOnDolGuldur $ fromJustNote "empty table" $ nonEmpty seats) (const $ pure ()) 1
  runGameT env do
    let shared = fromJustNote "invalid Dol Guldur event" $ newDolGuldurEvent $ size * 3
        replica number = EpicReplica (GroupOrdinal number) shared [] [] [] mempty mempty Nothing
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = replica <$> ordinal})
    run Setup
    finishSetupChoices 100
    body

spec :: Spec
spec = describe "Dol Guldur full native setup" do
  for_ (Nothing : map Just [0, 1, 2]) $ \ordinal ->
    for_ [1 .. 4] $ \size ->
      it ("runs complete opening setup for " <> show size <> " players at " <> show ordinal) . nativeSetupTable size ordinal $ do
        stage <- getActiveQuest
        toCardCode stage `shouldBe` toCardCode Quests.assaultOnTheGoldenWood
        stage.flipped `shouldBe` True
        stage.progress `shouldBe` 0
        countTokens Resource (toAttrs stage).questTokens `shouldBe` 0
        cityStrength `shouldReturn` (5 + 5 * size * if isJust ordinal then 3 else 1)
        attrs <- getsGame $ toAttrs . (.scenario)
        map toCardCode attrs.scenarioQuestDeck `shouldBe` map toCardCode
          [Quests.crossingTheAnduinDolGuldur, Quests.theSiegeOfDolGuldur, Quests.theTowerOfSorcery, Quests.battleUnderTheTrees, Quests.dungeonsOfTheNecromancer]
        sort (map toCardCode attrs.scenarioSetAside) `shouldBe` sort (map toCardCode
          [Objectives.hostOfLorien, Enemies.baneOfAmonLanc, Enemies.sorcererOfDolGuldurAttack, Enemies.chieftainMorlug, Enemies.dungeonDweller])
        entities <- getsGame (.entities)
        map toCardCode (toList entities.enemies) `shouldBe` replicate size (toCardCode Enemies.dolGuldurMarauder)
        toList entities.enemies `shouldSatisfy` all ((== StagingArea) . (.placement))
        map (enemyShadowCards . toAttrs) (toList entities.enemies) `shouldBe` replicate size []
        let holder = ordinal `notElem` [Just 1, Just 2]
            objectiveCodes = sort $ map toCardCode $ toList entities.objectives
            expected = sort $ map toCardCode $ Objectives.powerOfLorien : [Objectives.dolGuldurAttack | holder]
            supply = attrs.encounterDeck <> attrs.encounterDiscard <> attrs.scenarioSetAside
              <> [MkEncounterCard (toCard e).id (toCardCode e) | e <- toList entities.enemies]
              <> [MkEncounterCard (toCard l).id (toCardCode l) | l <- toList entities.locations]
              <> [MkEncounterCard (toCard o).id (toCardCode o) | o <- toList entities.objectives]
        objectiveCodes `shouldBe` expected
        length supply `shouldBe` if holder then 47 else 46
        length (nub $ map (.id) supply) `shouldBe` length supply
        supply `shouldSatisfy` all (\card -> cdCardType (toCardDef card) `notElem` [QuestType, SetupType])
        getsGame (.outcome) `shouldReturn` Nothing
