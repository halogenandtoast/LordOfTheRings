module LordOfTheRings.Scenario.Scenarios.TheThreeTrials (theThreeTrials) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo, CannotTakeDamage, EngagedEnemiesAddThreat, ImmuneToPlayerCardEffects))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers
import LordOfTheRings.Trait (Trait (Barrow, Guardian, Key))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window

newtype TheThreeTrials = TheThreeTrials ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theThreeTrials :: Scenario
theThreeTrials =
  Scenario
    $ scenario (TheThreeTrials . (nightmareDeckL ?~ nightmare)) "the-three-trials" "The Three Trials"
 where
  nightmare =
    nightmareDeck
      Nightmare.theThreeTrialsNightmare
      EncounterSet.TheThreeTrialsNightmare
      [ (Enemies.spiritOfTheWild, 3)
      , (Locations.grimFoothills, 3)
      , (Locations.tangledWoods, 1)
      , (Treacheries.turnedAround, 3)
      ]

{- | Nightmare: "Each engaged Guardian enemy contributes its threat to the total threat in
the staging area. While the Trial of Intuition is the current trial, each Guardian enemy
cannot be damaged, and each Barrow location cannot be the active location and is immune
to player card effects."
-}
instance HasModifiersFor TheThreeTrials where
  getModifiersFor (TheThreeTrials a) = when a.nightmare do
    modified a GameTarget [EngagedEnemiesAddThreat (EnemyWithTrait Guardian)]
    intuition <- getsGame (any ((== toCardCode Quests.theTrialOfIntuition) . toCardCode) . toList . (.entities.quests))
    when intuition do
      guardians <- select (EnemyWithTrait Guardian)
      for_ guardians \g -> modified a g [CannotTakeDamage]
      barrows <- select (LocationWithTrait Barrow)
      for_ barrows \b -> modified a b [CannotBeTraveledTo, ImmuneToPlayerCardEffects]

{- | Setup (The Trials Begin): Set aside all 3 Guardian enemies, all 3 Key objectives, all 3
Barrow locations, and Hallowed Circle.

The players choose which stage 2 trial to advance to, and advance to stage 3 once all
three are defeated. A Key discarded from the encounter deck goes to the staging area.
-}
instance RunMessage TheThreeTrials where
  runMessage msg (TheThreeTrials attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheThreeTrials, EncounterSet.AncientForest]
      let setAsideCard c =
            cdEncounterSet (toCardDef c) == Just EncounterSet.TheThreeTrials
              && ( any (`member` cdCardTraits (toCardDef c)) [Guardian, Key, Barrow]
                     || toCardCode c == toCardCode Locations.hallowedCircle
                 )
          (aside, encounterDeck) = partition setAsideCard cards
      firstStage <- genEncounterCard Quests.theTrialsBegin
      laterStages <-
        traverse
          genEncounterCard
          [Quests.theTrialOfStrength, Quests.theTrialOfPerseverance, Quests.theTrialOfIntuition, Quests.theAntleredCrownTheThreeTrials]
      putQuestIntoPlay firstStage
      pure $ TheThreeTrials $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages & setAsideL .~ aside
    -- "advance to a stage 2A of the players' choice": the first player chooses for them
    AdvanceQuest | trials@(_ : _) <- filter isTrial attrs.questDeck -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "threeTrials.chooseTrial"
        showing (map EncounterCard trials)
        targets trials \t -> push $ AdvanceToQuest t
      pure $ TheThreeTrials $ attrs & metaL .~ toJSON (Meta Nothing)
    ScenarioSpecific "threeTrials.currentKey" (parseMaybe parseJSON -> Just code) ->
      pure $ TheThreeTrials $ attrs & metaL .~ toJSON (Meta $ Just code)
    ScenarioSpecific "threeTrials.revealSetAside" (parseMaybe parseJSON -> Just card) -> do
      push $ RevealCard card
      pure $ TheThreeTrials $ attrs & setAsideL %~ filter (/= card)
    ScenarioSpecific "threeTrials.revealFromVictoryDisplay" (parseMaybe parseJSON -> Just card) -> do
      push $ RevealCard card
      pure $ TheThreeTrials $ attrs & victoryDisplayL %~ filter ((/= card.id) . (.id))
    ScenarioSpecific "threeTrials.attachKey" (parseMaybe parseJSON -> Just (key, hostCard)) -> do
      enemies <- select AnyEnemy
      hosts <- filterM (fmap ((== hostCard) . (.cardId) . toAttrs) . getEnemy) enemies
      locations <- select AnyLocation
      hostLocations <- filterM (fmap ((== hostCard) . (.cardId) . toAttrs) . getLocation) locations
      for_ (headMay $ map toTarget hosts <> map toTarget hostLocations) $ putObjectiveIntoPlayAttached key
      pure $ TheThreeTrials $ attrs & setAsideL %~ filter (/= key)
    ScenarioSpecific "threeTrials.bottomTen" (parseMaybe parseJSON -> Just card) -> do
      let (top, bottom) = splitAt (length attrs.encounterDeck - 10) attrs.encounterDeck
      bottom' <- shuffleM (card : bottom)
      pure $ TheThreeTrials $ attrs & setAsideL %~ filter (/= card) & encounterDeckL .~ top <> bottom'
    ScenarioSpecific "threeTrials.engageGuardians" _ -> do
      stages <- getsGame (map toCardDef . toList . (.entities.quests))
      let trial = any isTrial stages
          finalStage = any ((== toCardCode Quests.theAntleredCrownTheThreeTrials) . toCardCode) stages
      guardians <- select (EnemyWithTrait Guardian)
      for_ guardians \eid -> do
        e <- getEnemy eid
        target <-
          if
            | finalStage -> keyHolder (toCardDef e)
            | trial -> Just <$> getLead
            | otherwise -> pure Nothing
        for_ target \pid -> unless (e.placement == EngagedWith pid) $ engageEnemy pid eid
      pure $ TheThreeTrials attrs
    -- "Each Guardian enemy engages the first player" (each stage 2B), "...the player who
    -- controls the Key objective with a Trait shared by that enemy" (stage 3B): as one
    -- enters the staging area, the first player passes, or a Key is claimed
    CheckWindows ws | notNull [() | Window After (EntersStagingArea (EnemyTarget _)) <- ws] -> do
      engageGuardians
      TheThreeTrials <$> runMessage msg attrs
    SetFirstPlayer _ -> do
      engageGuardians
      TheThreeTrials <$> runMessage msg attrs
    ClaimObjective {} -> do
      engageGuardians
      TheThreeTrials <$> runMessage msg attrs
    -- "If Key of the Boar is discarded, add it to the staging area."
    AddToEncounterDiscard cards | found@(_ : _) <- filter isKey cards -> do
      traverse_ addToStaging found
      TheThreeTrials <$> runMessage (AddToEncounterDiscard $ filter (not . isKey) cards) attrs
    DiscardedTopOfEncounterDeck _ _ cards -> do
      for_ (filter isKey cards) \k -> do
        removeFromEncounterDiscard k
        addToStaging k
      TheThreeTrials <$> runMessage msg attrs
    _ -> TheThreeTrials <$> runMessage msg attrs
   where
    isTrial :: HasCardDef a => a -> Bool
    isTrial c = cdCardType (toCardDef c) == QuestType && cdStage (toCardDef c) == Just 2
