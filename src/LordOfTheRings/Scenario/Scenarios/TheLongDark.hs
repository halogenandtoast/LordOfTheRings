module LordOfTheRings.Scenario.Scenarios.TheLongDark (theLongDark) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Matcher (AllyMatcher (AllyControlledBy), PlayerMatcher (AnyPlayer), getLead, getPlayer, select)
import LordOfTheRings.Modifier (ModifierType (GainKeyword))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (sourceTarget)
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (lostEffectsInPlay, lostTriggered, passesLocateTest)

newtype TheLongDark = TheLongDark ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLongDark :: Scenario
theLongDark = Scenario $ scenario (TheLongDark . (nightmareDeckL ?~ nightmare)) "the-long-dark" "The Long Dark"
 where
  nightmare =
    nightmareDeck
      Nightmare.theLongDarkNightmare
      EncounterSet.TheLongDarkNightmare
      [ (Enemies.strayGoblin, 2)
      , (Treacheries.chanceEncounter, 2)
      , (Locations.theMountainsRoots, 2)
      , (Locations.abandonedMine, 1)
      , (Treacheries.fatigue, 2)
      , (Enemies.goblinSneak, 4)
      , (Enemies.rockAdder, 3)
      , (Enemies.goblinFollower, 4)
      , (Locations.branchingPaths, 1)
      , (Treacheries.burningLow, 3)
      ]

-- | Nightmare: "Durin's Greaves gains surge."
instance HasModifiersFor TheLongDark where
  getModifiersFor (TheLongDark a) = when a.nightmare do
    greaves <- getsGame (filter ((== toCardCode Objectives.durinsGreaves) . toCardCode) . toList . (.entities.objectives))
    for_ greaves \g -> modified a g [GainKeyword Surge]

instance RunMessage TheLongDark where
  runMessage msg (TheLongDark attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheLongDark, EncounterSet.TwistsAndTurns, EncounterSet.DeepsOfMoria]
      firstStage <- genEncounterCard Quests.journeyInTheBlackPit
      laterStages <- traverse genEncounterCard [Quests.continuingEastward]
      putQuestIntoPlay firstStage
      pure $ TheLongDark $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- The first player may discard a card from his hand to discard the top card of the
    -- encounter deck, again and again until one carries the pass icon. Once he runs out
    -- of cards or stops, the test is failed.
    LocateTest source onFail _ -> do
      lead <- getLead
      hand <- discardableHand <$> getPlayer lead
      chooseOrRunOneM lead do
        questionLabeled "locateTest.discard"
        traverse_ causedBy (sourceTarget source)
        targets hand \c -> do
          discardFromHand lead c
          push $ DoStep 1 msg
        labeled "locateTest.fail" $ pushAll onFail
      pure $ TheLongDark attrs
    DoStep 1 test@(LocateTest source _ onPass) -> case attrs.encounterDeck of
      top : _ -> do
        push $ DiscardTopOfEncounterDeck source GameTarget 1
        if passesLocateTest top then pushAll onPass else push test
        pure $ TheLongDark attrs
      []
        | notNull attrs.encounterDiscard -> do
            deck <- shuffleM attrs.encounterDiscard
            push msg
            pure $ TheLongDark $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
        | otherwise -> do
            -- nothing is discarded, so no pass icon either
            push test
            pure $ TheLongDark attrs
    -- Nightmare: "Lost: The first player must choose and discard an ally in play."
    _ | attrs.nightmare && lostTriggered ScenarioTarget msg -> do
      lead <- getLead
      allies <- select (AllyControlledBy AnyPlayer)
      chooseOrRunOneM lead do
        questionLabeled "theLongDark.discardAlly"
        targets allies (discard attrs)
      pure $ TheLongDark attrs
    -- each player in turn triggers a "Lost:" effect in play not already chosen
    ChooseLostEffects source (pid : rest) chosen -> do
      lost <- filter (`notElem` chosen) <$> lostEffectsInPlay
      chooseOrRunOneM pid do
        questionLabeled "theLongDark.triggerLost"
        traverse_ causedBy (sourceTarget source)
        targets lost \t -> do
          push $ TriggerLostEffectOf source t
          push $ ChooseLostEffects source rest (t : chosen)
      pure $ TheLongDark attrs
    _ -> TheLongDark <$> runMessage msg attrs
