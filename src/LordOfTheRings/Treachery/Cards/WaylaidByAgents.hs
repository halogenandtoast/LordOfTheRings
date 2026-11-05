module LordOfTheRings.Treachery.Cards.WaylaidByAgents (waylaidByAgents) where

import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (anyThreatAtMost20, canRemoveQuestTimeCounter)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WaylaidByAgents = WaylaidByAgents TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

waylaidByAgents :: TreacheryCard WaylaidByAgents
waylaidByAgents = treachery WaylaidByAgents Cards.waylaidByAgents

-- | When Revealed: Each player must choose: either remove 1 time counter from the quest or
-- discard an ally he controls from play. Then, if any player's threat is 20 or less,
-- Waylaid by Agents gains doomed 3.
-- Shadow: Attacking enemy gets +1 attack (+3 attack instead if your threat is 20 or less).
instance RunMessage WaylaidByAgents where
  runMessage msg t@(WaylaidByAgents attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        removable <- canRemoveQuestTimeCounter
        allies <- select (AllyControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "waylaidByAgents.choose"
          causedBy attrs
          when removable $ labeled "waylaidByAgents.removeTimeCounter" $ removeTimeCountersFromCurrentQuest attrs 1
          targets allies (discard attrs)
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      whenM anyThreatAtMost20 $ raiseThreatFromDoomed 3
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      threat <- (.threat) <$> getPlayer pid
      enemyGetsAttack attrs eid (if threat <= 20 then 3 else 1)
      pure t
    _ -> WaylaidByAgents <$> runMessage msg attrs
