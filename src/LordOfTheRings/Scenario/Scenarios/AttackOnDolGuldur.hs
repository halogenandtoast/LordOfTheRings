module LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Epic.DolGuldur
import LordOfTheRings.Game.Base (Game (gameRound))
import LordOfTheRings.Epic.Types
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Ability
import LordOfTheRings.Modifier (ModifierType (WinAtRoundEndWithDolGuldurCaptured, LoseKeyword))
import LordOfTheRings.Source
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Matcher (getMainQuestCards, select, EnemyMatcher (EnemyWithTrait), getLead, getPlayerOrder)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers
import LordOfTheRings.Trait (Trait (Lieutenant))
import LordOfTheRings.Token (Token (Resource))
import LordOfTheRings.Window

newtype AttackOnDolGuldur = AttackOnDolGuldur ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

attackOnDolGuldur :: Scenario
attackOnDolGuldur = Scenario $ scenario AttackOnDolGuldur "attack-on-dol-guldur" "Attack on Dol Guldur"

instance HasModifiersFor AttackOnDolGuldur where
  getModifiersFor (AttackOnDolGuldur a) = do
    modified a GameTarget [WinAtRoundEndWithDolGuldurCaptured]
    strength <- cityStrength
    when (strength == 0) do
      lieutenants <- select $ EnemyWithTrait Lieutenant
      for_ lieutenants $ \eid -> modified a eid [LoseKeyword Indestructible]

instance HasAbilities AttackOnDolGuldur where
  getAbilities (AttackOnDolGuldur a) = [mkAbility a 1 $ forced DolGuldurSiegeResolvedWindow]

withMeta :: Meta -> ScenarioAttrs -> ScenarioAttrs
withMeta meta a = a & metaL .~ toJSON meta

finalStages :: [CardDef]
finalStages = [Quests.theSiegeOfDolGuldur, Quests.theTowerOfSorcery, Quests.battleUnderTheTrees, Quests.dungeonsOfTheNecromancer]

instance RunMessage AttackOnDolGuldur where
  runMessage msg s@(AttackOnDolGuldur a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.AttackOnDolGuldur]
      putQuestIntoPlay =<< genEncounterCard Quests.assaultOnTheGoldenWood
      later <- traverse genEncounterCard $ Quests.crossingTheAnduinDolGuldur : finalStages
      pure $ AttackOnDolGuldur $ withMeta emptyMeta $ a & encounterDeckL .~ deck & questDeckL .~ later
    DoStep 6 Setup -> do
      players <- getPlayerOrder
      let replica = a.scenarioEpic
          holder = maybe True ((== GroupOrdinal 0) . epicGroup) replica
      for_ (find ((== toCardCode Objectives.dolGuldurAttack) . toCardCode) a.encounterDeck) $ \card -> do
        removeFromEncounterDeck card
        if holder then do
          addToStaging card
          push $ ScenarioSpecific "dolGuldur.initializeCity" $ toJSON $ 5 + 5 * length players
        else removeFromGame $ CardIdTarget card.id
      pure s
    ScenarioSpecific "dolGuldur.initializeCity" value
      | Just amount <- parseMaybe parseJSON value -> do
          cities <- getsGame $ filter ((== toCardCode Objectives.dolGuldurAttack) . toCardCode) . toList . (.entities.objectives)
          for_ cities $ \city -> push $ DoStep 1 $ PlaceTokens GameSource (toTarget city) Resource amount
          pure s
    CheckWindows windows -> do
      -- ExploredCard and EnemyDestroyed report the physical card after it
      -- leaves play. A discard without defeat/exploration contributes no Siege.
      let amount = sum
            [siegeValue $ toCardCode card
            | Window After windowType <- windows
            , card <- case windowType of
                ExploredCard card -> [card]
                EnemyDestroyed card -> [card]
                _ -> []]
      when (amount > 0 && isJust a.scenarioEpic) $ push $ ContributeShared DolGuldurSiegeTracked amount
      pure $ AttackOnDolGuldur $ withMeta (meta {roundSiege = meta.roundSiege + amount}) a
    SyncEpic shared -> do
      when (dolGuldurCityStrength shared == Just 0) do
        foes <- select $ EnemyWithTrait Lieutenant
        traverse_ (checkDefeated a) foes
      AttackOnDolGuldur <$> runMessage msg a
    BeginRound -> pure $ AttackOnDolGuldur $ withMeta emptyMeta a
    ScenarioSpecific "dolGuldur.stageForcedResolved" value
      | Just code <- parseMaybe parseJSON value -> do
          let resolved = nub $ code : meta.stageForcesResolved
          when (isNothing a.scenarioEpic) $ push $ CheckWindows [mkWhen DolGuldurStageEffectsResolved]
          pure $ AttackOnDolGuldur $ withMeta (meta {stageForcesResolved = resolved}) a
    ScenarioSpecific "dolGuldur.resolveSiege" _ -> do
      let amount = maybe meta.roundSiege (sharedCounter DolGuldurSiegeTracked . epicShared) a.scenarioEpic
      damageDolGuldur a amount
      when (isJust a.scenarioEpic) $ push $ ContributeShared DolGuldurSiegeTracked $ negate amount
      pure $ AttackOnDolGuldur $ withMeta (meta {roundSiege = 0}) a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      let active = maybe [] (concatMap (maybe [] (map epicQuestCard . epicStageQuests) . epicBoardStage) . epicBoards) a.scenarioEpic
      quests <- getMainQuestCards
      let used = nub $ map toCardCode quests <> active
          unused = filter ((`notElem` used) . toCardCode) finalStages
      chosen <- headMay <$> shuffleM unused
      for_ chosen $ \stage -> stagePenalty $ toCardCode stage
      push $ ScenarioSpecific (if isJust a.scenarioEpic then "dolGuldur.chooseAllStages" else "dolGuldur.chooseStage") Null
      pure s
    ScenarioSpecific "dolGuldur.chooseStage" _ -> do
      lead <- getLead
      current <- getMainQuestCards
      let coordinated = do
            replica <- a.scenarioEpic
            state <- replica.epicShared.dolGuldurCoordination
            either (const Nothing) Just $ availableDolStages replica.epicGroup state.epicDolTransition
          candidates = [card | card <- a.questDeck, any ((== toCardCode card) . toCardCode) finalStages
            , toCardCode card `notElem` map toCardCode current
            , maybe True (toCardCode card `elem`) coordinated]
      chooseOneM lead do
        questionLabeled "dolGuldur.chooseStage"
        causedBy a
        targets candidates $ \card -> push $ ScenarioSpecific
          (if isJust a.scenarioEpic then "dolGuldur.claimStage" else "dolGuldur.advanceToStage") $ toJSON $ toCardCode card
      pure s
    ScenarioSpecific "dolGuldur.claimStage" value
      | Just code <- parseMaybe parseJSON value
      , Just _ <- a.scenarioEpic -> do
          current <- getMainQuestCards
          roundNumber <- getsGame gameRound
          let kind = if any ((== Just 2) . cdStage . toCardDef) current then DolInitialStages else DolRotateStages
          push $ RequestEpicEffect (GroupOrdinal 0) code
            (EpicSubmitDolClaim $ DolClaim (DolEpoch roundNumber kind) code) ScenarioTarget
          pure s
    ScenarioSpecific "dolGuldur.advanceToStage" value
      | Just code <- parseMaybe parseJSON value -> do
          current <- getMainQuestCards
          for_ (find ((== code) . toCardCode) a.questDeck) $ \card -> do
            for_ current $ \stage -> do
              removeStageFromPlay $ toId stage
              when (cdStage (toCardDef stage) == Just 3) $ shuffleIntoQuestDeck stage
            advanceToQuest card
          pure s
    _ -> AttackOnDolGuldur <$> runMessage msg a
   where
    meta = getMeta a
