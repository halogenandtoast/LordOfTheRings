module LordOfTheRings.Event.Cards.JusticeShallBeDone (justiceShallBeDone) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype JusticeShallBeDone = JusticeShallBeDone EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Limit 1 per deck, on its card def.
justiceShallBeDone :: EventCard JusticeShallBeDone
justiceShallBeDone = eventWith JusticeShallBeDone Cards.justiceShallBeDone (criteriaL .~ DuringPhase PlanningPhase)

-- | Planning Action: Add Justice Shall Be Done to the victory display to draw 3 cards and
-- add 3 resources to the resource pool of each hero you control. At the end of the round,
-- you are eliminated from the game.
instance RunMessage JusticeShallBeDone where
  runMessage msg e@(JusticeShallBeDone attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      addToVictoryDisplay card
      drawCards attrs pid 3
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> placeResources attrs h 3
      roundModifier attrs (PlayerTarget pid) EliminatedAtEndOfRound
      pure e
    _ -> JusticeShallBeDone <$> runMessage msg attrs
