module LordOfTheRings.Quest.Cards.TheLastGasp (theLastGasp) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Scenarios.MountDoom.Helpers (firstTreacheryImmune, firstTreacherySurges)

newtype TheLastGasp = TheLastGasp QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theLastGasp :: QuestCard TheLastGasp
theLastGasp = quest TheLastGasp Cards.theLastGasp

-- | Dire. The first treachery revealed each quest phase is immune to player card effects
-- and gains surge.
instance HasModifiersFor TheLastGasp where
  getModifiersFor (TheLastGasp a) = when a.flipped $ firstTreacheryImmune a

-- | Forced: At the beginning of the quest phase, the first player makes a Fortitude test. If
-- the first player fails this test, no more than 5 progress can be placed here this round.
instance HasAbilities TheLastGasp where
  getAbilities (TheLastGasp a) =
    [ ability
    | a.flipped
    , ability <- [firstTreacherySurges a 1, mkAbility a 2 $ forced $ PhaseBeginsWindow After QuestPhase]
    ]

mountDoomActive :: QueueT Message GameT Bool
mountDoomActive = selectAny (IsActiveLocation <> LocationWithCardCode (toCardCode Locations.mountDoom))

{- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
cards from the top until X locations are discarded, where X is the number of players in
the game. Add each location discarded this way to the staging area.

This stage cannot be defeated unless Mount Doom is the active location.
-}
instance RunMessage TheLastGasp where
  runMessage msg q@(TheLastGasp attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheLastGasp <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    -- the deck is read once it is shuffled
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType LocationType) cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      lead <- getLead
      fortitudeTest lead attrs (roundModifier attrs attrs (ProgressPerRoundLimit 5)) (pure ())
      pure q
    -- Mount Doom becoming the active location may meet the condition
    PlaceLocation _ _ -> do
      push $ CheckStage attrs.id
      TheLastGasp <$> runConditionalStage mountDoomActive msg attrs
    _ -> TheLastGasp <$> runConditionalStage mountDoomActive msg attrs
