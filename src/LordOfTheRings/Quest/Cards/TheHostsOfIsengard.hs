module LordOfTheRings.Quest.Cards.TheHostsOfIsengard (theHostsOfIsengard) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (firstPlayerReveals)

newtype TheHostsOfIsengard = TheHostsOfIsengard QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Defense, as its card def says.
theHostsOfIsengard :: QuestCard TheHostsOfIsengard
theHostsOfIsengard = quest TheHostsOfIsengard Cards.theHostsOfIsengard

{- | Forced: After resolving the quest, if the encounter deck did not place at least 1
progress on this stage as the result of questing, the first player reveals 1 encounter
card. What questing placed on the stage itself marks it for the phase (see
'ProgressFromQuestingThisPhase'): progress the active location took does not count.
-}
instance HasAbilities TheHostsOfIsengard where
  getAbilities (TheHostsOfIsengard a) =
    [ restricted a 1 (TargetLacksModifier (toTarget a) ProgressFromQuestingThisPhase) $ forced QuestResolvedWindow
    | a.flipped
    ]

{- | When Revealed: Search the encounter deck, discard pile, and victory display for Helm's
Gate and add it to the staging area. Shuffle the encounter deck. Make Helm's Gate the
active location, returning any previous active location to the staging area.
-}
instance RunMessage TheHostsOfIsengard where
  runMessage msg q@(TheHostsOfIsengard attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheHostsOfIsengard <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searchEncounterCardsAndVictoryDisplay (cardIs Locations.helmsGate) \gate -> do
        addToStaging gate
        doStep 1 (WithTarget (CardIdTarget gate.id) msg)
      pure q
    DoStep 1 (WithTarget (CardIdTarget cid) (WhenRevealed (isTarget attrs -> True))) -> do
      gates <- getsGame (map (.id) . filter ((== cid) . (.cardId) . toAttrs) . toList . (.entities.locations))
      for_ gates \gate -> do
        actives <- select IsActiveLocation
        for_ actives \l -> placeLocation l StagingArea
        makeActive gate
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      firstPlayerReveals
      pure q
    _ -> TheHostsOfIsengard <$> runMessage msg attrs
