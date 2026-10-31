module LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood (returnToMirkwood) where

import LordOfTheRings.Ability (forced, mkAbility)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (HeroControlledBy, ReadyHero), PlayerMatcher (PlayerWithId), getEncounterDiscard, getLead, select)
import LordOfTheRings.Matcher.Types (cardIs)
import LordOfTheRings.Matcher.Window (WindowMatcher (PhaseBeginsWindow))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Trait (Trait (Tantrum))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype ReturnToMirkwood = ReturnToMirkwood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

returnToMirkwood :: Scenario
returnToMirkwood =
  Scenario
    $ scenario (ReturnToMirkwood . (nightmareDeckL ?~ nightmare)) "return-to-mirkwood" "Return to Mirkwood"
 where
  nightmare =
    nightmareDeck
      Nightmare.returnToMirkwoodNightmare
      EncounterSet.ReturnToMirkwoodNightmare
      [ (Locations.woodmansGlade, 3)
      , (Locations.woodElfPath, 3)
      , (Locations.mountainsOfMirkwood, 3)
      , (Enemies.goblinSniper, 2)
      , (Enemies.marshAdder, 1)
      , (Enemies.wargs, 2)
      , (Treacheries.despair, 2)
      , (Locations.theBrownLands, 2)
      , (Locations.theEastBight, 2)
      ]

-- | Nightmare: "Forced: At the beginning of the quest phase, the player who is guarding
-- Gollum must either exhaust a hero he controls or resolve the "when revealed" effect of
-- the topmost Tantrum card in the encounter discard pile."
instance HasAbilities ReturnToMirkwood where
  getAbilities (ReturnToMirkwood a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.nightmare]

instance RunMessage ReturnToMirkwood where
  runMessage msg (ReturnToMirkwood attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.ReturnToMirkwood, EncounterSet.SpidersOfMirkwood, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.throughTheForest
      laterStages <- traverse genEncounterCard [Quests.escapeAttempt, Quests.toTheElvenKingsHalls, Quests.ambush]
      putQuestIntoPlay firstStage
      pure $ ReturnToMirkwood $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    UseCardAbility _ ScenarioSource 1 _ -> do
      tantrum <- find ((Tantrum `member`) . cdCardTraits . toCardDef) <$> getEncounterDiscard
      getGuard >>= traverse_ \guard' -> do
        heroes <- select (HeroControlledBy (PlayerWithId guard') <> ReadyHero)
        chooseOrRunOneM guard' do
          questionLabeled "returnToMirkwoodNightmare.tantrum"
          causedBy attrs
          targets heroes exhaust
          for_ tantrum \t -> labeled "returnToMirkwoodNightmare.resolveTantrum" $ resolveWhenRevealedFromDiscard t
      pure $ ReturnToMirkwood attrs
    -- Nightmare: "Forced: When Stage 4A is revealed, search the encounter deck and discard
    -- pile for 1 copy of Attercop, Attercop and add it to the staging area, if able."
    AdvanceToQuest card | attrs.nightmare, toCardCode card == toCardCode Quests.ambush -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Enemies.attercopAttercop)
      ReturnToMirkwood <$> runMessage msg attrs
    _ -> ReturnToMirkwood <$> runMessage msg attrs
