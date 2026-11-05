module LordOfTheRings.Quest.Cards.TheVoiceOfSaruman (theVoiceOfSaruman) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (cancelQuestProgress, questProgressCanceled, questProgressThisPhase, recordQuestProgress)

newtype TheVoiceOfSaruman = TheVoiceOfSaruman QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theVoiceOfSaruman :: QuestCard TheVoiceOfSaruman
theVoiceOfSaruman = quest TheVoiceOfSaruman Cards.theVoiceOfSaruman

-- | While Orthanc is the active location, Saruman is considered to be engaged with the first
-- player: attacking him, and attacked by his characters alone.
instance HasModifiersFor TheVoiceOfSaruman where
  getModifiersFor (TheVoiceOfSaruman a) = when a.flipped do
    orthancActive <- selectAny (LocationWithCardCode (toCardCode Locations.orthanc) <> IsActiveLocation)
    when orthancActive do
      lead <- getLead
      others <- filter (/= lead) <$> getRemainingPlayers
      sarumans <- select (EnemyWithCardCode $ toCardCode Enemies.sarumanTheRoadToIsengard)
      for_ sarumans \e -> modified a e $ EngagedWithEachPlayer FirstPlayer : [CannotBeAttackedByCharactersOf p | p <- others]

-- | Forced: When Orthanc is explored, cancel any progress placed on the quest this phase and
-- return Orthanc to the staging area.
instance HasAbilities TheVoiceOfSaruman where
  getAbilities (TheVoiceOfSaruman a) = [mkAbility a 1 $ forced $ AnyExploredWindow When | a.flipped]

{- | When Revealed: Each player draws 6 cards. Each player with more than 6 cards in his hand
discards cards from his hand until he has only 6. Attach Saruman's Staff to Saruman.

The stage turns over once its When Revealed has resolved, so a player is not eliminated
for the empty hand he has before he draws.

If a player has 0 cards in his hand, he is eliminated from the game: read as each moment
passes. If the players defeat this stage, they win the game: it is the last.

The progress placed on this stage this phase is kept by the scenario. Orthanc is explored
as questing's progress is placed on it, before the rest goes on this stage, as it would
all have been placed at once: the rest of that progress is canceled too, any placed by
questing after Orthanc is explored this phase.
-}
instance RunMessage TheVoiceOfSaruman where
  runMessage msg q@(TheVoiceOfSaruman attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      pure q
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> drawCards attrs p 6
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      eachPlayer \p -> do
        hand <- (.hand) <$> getPlayer p
        when (length hand > 6) $ chooseAndDiscardFromHand p attrs (length hand - 6)
      staff <- filter (cardMatches $ cardIs Objectives.sarumansStaffTheRoadToIsengard) <$> getSetAside AnyCard
      sarumans <- select (EnemyWithCardCode $ toCardCode Enemies.sarumanTheRoadToIsengard)
      for_ (take 1 sarumans) \saruman -> for_ staff \c -> putObjectiveIntoPlayAttached c (EnemyTarget saruman)
      doStep 2 (WhenRevealed $ toTarget attrs)
      pure q
    DoStep 2 (WhenRevealed (isTarget attrs -> True)) ->
      TheVoiceOfSaruman <$> runMessage (Flip $ toTarget attrs) attrs
    PlaceTokens GameSource (isTarget attrs -> True) Progress _ | attrs.flipped -> do
      canceled <- questProgressCanceled
      if canceled then pure q else tracked
    PlaceTokens _ (isTarget attrs -> True) Progress _ | attrs.flipped -> tracked
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      orthancs <- select (LocationWithCardCode $ toCardCode Locations.orthanc)
      for_ [lid | Window _ (Explored lid) <- ws, lid `elem` orthancs] \lid -> do
        placed <- questProgressThisPhase
        when (placed > 0) $ removeTokens attrs attrs Progress (min placed attrs.progress)
        cancelQuestProgress
        cancelMatching \case
          Discard _ (LocationTarget lid') -> lid' == lid
          _ -> False
        orthanc <- getLocation lid
        removeTokens attrs lid Progress orthanc.progress
        placeLocation lid StagingArea
      pure q
    CheckWindows _ | attrs.flipped -> do
      players <- getRemainingPlayers
      for_ players \p -> do
        hand <- (.hand) <$> getPlayer p
        when (null hand) $ eliminatePlayer p
      TheVoiceOfSaruman <$> runMessage msg attrs
    _ -> TheVoiceOfSaruman <$> runMessage msg attrs
   where
    tracked = case msg of
      PlaceTokens _ _ Progress n -> do
        recordQuestProgress n
        TheVoiceOfSaruman <$> runMessage msg attrs
      _ -> TheVoiceOfSaruman <$> runMessage msg attrs
