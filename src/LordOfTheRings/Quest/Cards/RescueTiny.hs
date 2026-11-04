module LordOfTheRings.Quest.Cards.RescueTiny (rescueTiny) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RoamAcrossRhovanion.Helpers (firstPlayerControls, guardOf, searchAndGuard)

newtype RescueTiny = RescueTiny QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

rescueTiny :: QuestCard RescueTiny
rescueTiny = quest RescueTiny Cards.rescueTiny

-- | The 'when revealed' effects of treachery cards cannot be canceled.
-- The players cannot travel to the Grey Moorland guarding Tiny unless there is at least 5
-- progress on this stage.
instance HasModifiersFor RescueTiny where
  getModifiersFor (RescueTiny a) = when a.flipped do
    treacheries <- getsGame (toList . (.entities.treacheries))
    for_ treacheries \t -> modified a t [WhenRevealedCannotBeCanceled]
    when (a.progress < 5) do
      guard' <- guardOf Objectives.tiny
      locations <- getsGame (filter ((== guard') . Just . (.id) . toCard) . toList . (.entities.locations))
      for_ locations \l -> modified a l [CannotBeTraveledTo]

-- | This stage cannot be defeated unless the first player controls Urdug and Tiny.
defeatable :: HasGame m => m Bool
defeatable = (&&) <$> firstPlayerControls Objectives.urdug <*> firstPlayerControls Objectives.tiny

{- | When Revealed: The first player searches the encounter deck, discard pile, and victory
display for a Grey Moorland, adds it to the staging area, and attaches the set-aside Tiny
objective-ally to it as a guarded objective. Shuffle the encounter deck. Each other player
reveals an encounter card.
Forced: When the players defeat this stage, remove it from the game. Advance to a random
stage 2A, if able. Otherwise, advance to stage 3A: a defeated stage leaves play for good,
and the scenario draws the next from the lowest stage left.
-}
instance RunMessage RescueTiny where
  runMessage msg q@(RescueTiny attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RescueTiny <$> runConditionalStage defeatable msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searchAndGuard Locations.greyMoorland Objectives.tiny
      pure q
    RescueCharacter {} -> recheck
    SetFirstPlayer {} -> recheck
    _ -> RescueTiny <$> runConditionalStage defeatable msg attrs
   where
    recheck = do
      checkStage attrs.id
      RescueTiny <$> runConditionalStage defeatable msg attrs
