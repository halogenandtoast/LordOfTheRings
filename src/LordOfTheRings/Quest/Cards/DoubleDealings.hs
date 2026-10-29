module LordOfTheRings.Quest.Cards.DoubleDealings (doubleDealings) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers

newtype DoubleDealings = DoubleDealings QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

doubleDealings :: QuestCard DoubleDealings
doubleDealings = quest DoubleDealings Cards.doubleDealings

instance HasModifiersFor DoubleDealings where
  getModifiersFor (DoubleDealings a) = eliminationLevelsLowered a

-- | Time 4. Forced: After the last time counter is removed from this quest, reduce each
-- player's threat elimination level by 10 for the remainder of the game. Then, place 4 time
-- counters on this stage.
instance HasAbilities DoubleDealings where
  getAbilities (DoubleDealings a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a, emptyMugAbility a 2, mkAbility a 3 $ forced $ ProgressPlacedWindow After (toTarget a)]

-- | Setup: Set Bellach and The Crossing at Tharbad aside, out of play. Search the encounter
-- deck for 1 copy of Spy from Mordor per player, and add them to the staging area. Make The
-- Empty Mug the active location. The first player takes control of Nalir, as an ally.
-- Shuffle the encounter deck.
-- Forced: After one or more progress is placed on this quest, cancel all progress and reduce
-- each player's threat by an equal amount. When all players have 0 threat, immediately
-- proceed to stage 2.
-- The stage has no quest points, so the progress cannot defeat it before it is canceled.
instance RunMessage DoubleDealings where
  runMessage msg q@(DoubleDealings attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let copies def = filter (cardMatches $ cardIs def) deck
      traverse_ setAside $ concatMap copies [Enemies.bellachTroubleInTharbad, Locations.theCrossingAtTharbad]
      for_ (take players $ copies Enemies.spyFromMordor) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      for_ (take 1 $ copies Locations.theEmptyMug) \card -> do
        removeFromEncounterDeck card
        putIntoPlayAsActiveLocation card
      -- Nalir joins the first player as he comes out
      for_ (take 1 $ copies Objectives.nalirTroubleInTharbad) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      pure $ DoubleDealings $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lowerEliminationLevels
      placeTimeCounters attrs attrs 4
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      removeCardThatReducedThreat ws
      pure q
    UseCardAbility _ (isSource attrs -> True) 3 _ | attrs.progress > 0 -> do
      removeTokens attrs attrs Progress attrs.progress
      eachPlayer \p -> lowerThreat p attrs.progress
      pure q
    -- a reduced threat may be the last at 0
    LowerThreat {} -> do
      checkStage attrs.id
      DoubleDealings <$> runMessage msg attrs
    CheckStage qid | qid == attrs.id -> do
      zero <- allM (fmap ((== 0) . (.threat)) . getPlayer) =<< getRemainingPlayers
      when zero $ completeQuest attrs.id
      pure q
    _ -> DoubleDealings <$> runMessage msg attrs
