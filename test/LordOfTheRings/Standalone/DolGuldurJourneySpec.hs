module LordOfTheRings.Standalone.DolGuldurJourneySpec (spec) where

import LordOfTheRings.Card (EncounterCard (..), IsCard (toCard), cdStage, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyShadowCards, enemyTokens)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur)
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers (cityStrength)
import LordOfTheRings.Scenario.Types (scenarioQuestDeck, scenarioSetAside)
import TestImport

-- The journey exercises the production Setup, EndRound, quest advancement,
-- reveal and search queues. At a round boundary runDebug delivers EndRound
-- ahead of the unrelated planning window; no encounter deck or quest is replaced.
finishSetup :: Int -> GameT ()
finishSetup remaining = do
  questions <- getsGame (.question)
  case headMay $ mapToList questions of
    Nothing -> pure ()
    Just (pid, _) | remaining > 0 -> do
      chooseOptionMatching "complete native Dol Guldur setup" pid (const True)
      finishSetup $ remaining - 1
    _ -> error "native setup did not settle"

promptOf :: Question Message -> Text
promptOf = \case
  PlayerWindow _ inner -> promptOf inner
  QuestionLabel label _ _ _ -> label
  _ -> ""

finishBoundary :: Bool -> CardDef -> CardDef -> Int -> GameT ()
finishBoundary keepSupport nextStage searchedLocation remaining = do
  questions <- getsGame (.question)
  case headMay $ mapToList questions of
    Nothing -> pure ()
    Just (pid, question)
      | promptOf question == "$label.planningActions" -> pure ()
      | remaining <= 0 -> error "Dol Guldur end-of-round chain did not settle"
      | otherwise -> do
          let prompt = promptOf question
          if
            | "$label.resolveWhenRevealed" `isPrefixOf` prompt ->
                chooseOptionMatching "resolve native quest revelation" pid (const True)
            | prompt == "$label.useResponse" -> chooseLabel "$label.pass" pid
            | prompt `elem` ["$label.takeAction", "$label.noRemainingActions"] ->
                chooseOptionMatching "decline resource-phase actions" pid \case
                  Label option _ -> option == "$label.pass" || "$label.proceedTo." `isPrefixOf` option
                  _ -> False
            | prompt == "$label.dolGuldur.claimHost" ->
                chooseLabel (if keepSupport then "$label.dolGuldur.takeHost" else "$label.skip") pid
            | prompt == "$label.dolGuldur.claimPower" ->
                chooseLabel "$label.dolGuldur.takePower" pid
            | prompt == "$label.searchChoose" -> do
                attrs <- getsGame $ toAttrs . (.scenario)
                let matching = find ((== toCardCode searchedLocation) . toCardCode) $ attrs.encounterDeck <> attrs.encounterDiscard
                chooseTarget pid $ fromJustNote "printed location search lost its target" matching
            | prompt == "$label.dolGuldur.chooseStage" -> do
                deck <- getsGame $ scenarioQuestDeck . toAttrs . (.scenario)
                chooseTarget pid $ fromJustNote "requested next stage is unavailable" $ find ((== toCardCode nextStage) . toCardCode) deck
            | prompt == "$label.dolGuldur.preventPenalty" -> chooseLabel "$label.dolGuldur.removeFive" pid
            | prompt == "$label.dolGuldur.stageBenefit" -> chooseLabel "$label.skip" pid
            | prompt == "$label.chooseCardToDiscardFromHand" ->
                chooseOptionMatching "resolve the unused Tower's hand discard" pid (const True)
            | prompt == "$label.dolGuldur.archer" ->
                chooseOptionMatching "resolve the newly engaged Archer" pid (const True)
            | prompt == "$label.dolGuldur.battleWarg" ->
                chooseOptionMatching "resolve the newly engaged Warg" pid (const True)
            | otherwise -> error $ "unexpected native boundary question: " <> show prompt
          finishBoundary keepSupport nextStage searchedLocation $ remaining - 1

encounterSupply :: GameT [EncounterCard]
encounterSupply = getsGame $ \g ->
  let attrs = toAttrs g.scenario
      encounter entity = MkEncounterCard (toCard entity).id $ toCardCode entity
   in attrs.encounterDeck <> attrs.encounterDiscard <> attrs.scenarioSetAside
      <> [card | EncounterCard card <- attrs.victoryDisplay]
      <> map encounter (toList g.entities.enemies)
      <> concatMap (enemyShadowCards . toAttrs) (toList g.entities.enemies)
      <> map encounter (toList g.entities.locations)
      <> map encounter (toList g.entities.objectives)
      <> map encounter (filter ((== toCardCode Objectives.hostOfLorien) . toCardCode) $ toList g.entities.allies)

finalQuestIds :: GameT [CardId]
finalQuestIds = getsGame $ \g -> sort $
  [card.id | card <- (toAttrs g.scenario).scenarioQuestDeck, cdStage (toCardDef card) == Just 3]
  <> [(toCard stage).id | stage <- toList g.entities.quests, cdStage (toCardDef stage) == Just 3]

assertSupply :: [CardId] -> GameT ()
assertSupply expected = do
  cards <- encounterSupply
  sort (map (.id) cards) `shouldBe` expected
  length (nub $ map (.id) cards) `shouldBe` 47

spec :: Spec
spec = describe "Dol Guldur native standard journeys" do
  for_ [(False, Quests.theSiegeOfDolGuldur, Locations.gateOfDolGuldurAttack, Enemies.baneOfAmonLanc), (True, Quests.dungeonsOfTheNecromancer, Locations.dungeonDoor, Enemies.dungeonDweller)] $ \(keepSupport, firstFinal, searchedLocation, firstLieutenant) ->
    it ("runs all three defensive rounds, Crossing the Anduin, and a full stage-three rotation; keep support=" <> show keepSupport) . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
      run Setup
      finishSetup 100
      initialSupply <- sort . map (.id) <$> encounterSupply
      initialFinals <- finalQuestIds
      length initialSupply `shouldBe` 47
      length initialFinals `shouldBe` 4
      cityStrength `shouldReturn` 10
      for_ [1 :: Int .. 3] $ \roundNumber -> do
        stage <- getActiveQuest
        toCardCode stage `shouldBe` toCardCode Quests.assaultOnTheGoldenWood
        void $ runDebug $ PlaceTokens GameSource (toTarget stage) Progress (if keepSupport then 10 else 0)
        (.progress) <$> getActiveQuest `shouldReturn` (if keepSupport then 10 else 0)
        void $ runDebug EndRound
        finishBoundary keepSupport firstFinal searchedLocation 100
        assertSupply initialSupply
        if roundNumber < 3 then do
          currentStage <- getActiveQuest
          toCardCode currentStage `shouldBe` toCardCode Quests.assaultOnTheGoldenWood
          countTokens Resource (toAttrs currentStage).questTokens `shouldBe` roundNumber
          currentStage.progress `shouldBe` 0
        else toCardCode <$> getActiveQuest `shouldReturn` toCardCode firstFinal
      finalQuestIds `shouldReturn` initialFinals
      -- A stage-three Forced effect cannot retroactively trigger in the
      -- stage-one end-of-round window in which it entered (FAQ 1.58).
      cityStrength `shouldReturn` 10
      firstStage <- getActiveQuest
      firstStage.progress `shouldBe` 0
      getsGame (length . (.entities.allies)) `shouldReturn` (if keepSupport then 1 else 0)
      getsGame (map (.placement) . filter ((== toCardCode Objectives.powerOfLorien) . toCardCode) . toList . (.entities.objectives)) `shouldReturn`
        [AttachedTo (PlayerTarget pid) | keepSupport]
      when (not keepSupport) $
        map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Objectives.powerOfLorien))
      lieutenant <- getsGame $ fromJustNote "missing first Lieutenant" . headMay . filter ((== toCardCode firstLieutenant) . toCardCode) . toList . (.entities.enemies)
      let oldLieutenantId = toId lieutenant
          oldLieutenantCardId = (toCard lieutenant).id
      void $ runDebug $ PlaceTokens GameSource (EnemyTarget oldLieutenantId) Damage 2
      void $ runDebug $ PlaceTokens GameSource (EnemyTarget oldLieutenantId) Resource 2
      searched <- getsGame $ fromJustNote "missing searched location" . headMay . filter ((== toCardCode searchedLocation) . toCardCode) . toList . (.entities.locations)
      void $ runDebug $ PlaceTokens GameSource (toTarget searched) Progress 4
      void $ runDebug $ PlaceTokens GameSource (toTarget firstStage) Progress 9
      (.progress) <$> getActiveQuest `shouldReturn` 9
      void $ runDebug EndRound
      finishBoundary keepSupport Quests.theTowerOfSorcery searchedLocation 100
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.theTowerOfSorcery
      (.progress) <$> getActiveQuest `shouldReturn` 0
      strength <- cityStrength
      strength `shouldSatisfy` (`elem` if keepSupport then [8, 11] else [8])
      foes <- getsGame $ toList . (.entities.enemies)
      map toId foes `shouldSatisfy` notElem oldLieutenantId
      newLieutenant <- pure $ fromJustNote "missing new Sorcerer" $ headMay $ filter ((== toCardCode Enemies.sorcererOfDolGuldurAttack) . toCardCode) foes
      fresh <- getEnemy $ toId newLieutenant
      countTokens Damage (enemyTokens $ toAttrs fresh) `shouldBe` 0
      countTokens Resource (enemyTokens $ toAttrs fresh) `shouldBe` 0
      aside <- getsGame $ scenarioSetAside . toAttrs . (.scenario)
      map (.id) aside `shouldSatisfy` elem oldLieutenantCardId
      finalQuestIds `shouldReturn` initialFinals
      assertSupply initialSupply
      getsGame (.outcome) `shouldReturn` Nothing
