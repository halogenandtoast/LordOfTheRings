module LordOfTheRings.Event.Cards.DoomHangsStill (doomHangsStill) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype DoomHangsStill = DoomHangsStill EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Planning Action: Until the end of the round, players do not raise their threat from
-- questing unsuccessfully.
-- Valour Planning Action: Raise each player's threat by 2 to skip the quest phase this
-- round.
doomHangsStill :: EventCard DoomHangsStill
doomHangsStill = eventWith DoomHangsStill Cards.doomHangsStill (criteriaL .~ DuringPhase PlanningPhase)

instance RunMessage DoomHangsStill where
  runMessage msg e@(DoomHangsStill attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      chooseOrRunOneM pid do
        questionLabeled "doomHangsStill.chooseOne"
        causedBy attrs
        labeled "doomHangsStill.noThreat" $ roundModifier attrs GameTarget NoThreatFromQuestingUnsuccessfully
        valourLabeled pid do
          eachPlayer \p -> raiseThreat p 2
          roundModifier attrs GameTarget SkipQuestPhase
      pure e
    _ -> DoomHangsStill <$> runMessage msg attrs
