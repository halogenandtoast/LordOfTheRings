module LordOfTheRings.Event.Cards.ManTheWalls (manTheWalls) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype ManTheWalls = ManTheWalls EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if no other copies of Man the Walls have been played this round.
manTheWalls :: EventCard ManTheWalls
manTheWalls =
  eventWith ManTheWalls Cards.manTheWalls
    $ criteriaL .~ (DuringPhase PlanningPhase <> NotCriterion (CardPlayedThisRound $ toCardCode Cards.manTheWalls))

-- | Planning Action: Reduce the cost of the first ally played by each player this phase by
-- 1. Those allies cannot quest this round. Each player's reduction is spent on their first
-- ally, and marks it.
instance RunMessage ManTheWalls where
  runMessage msg e@(ManTheWalls attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \pid ->
        phaseModifiers attrs (PlayerTarget pid) [LowerNextCostOf (CardWithType AllyType) 1, SpentOnCardGives [CannotCommitToQuest]]
      pure e
    _ -> ManTheWalls <$> runMessage msg attrs
