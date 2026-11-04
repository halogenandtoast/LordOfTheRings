module LordOfTheRings.Event.Cards.TheGreatHunt (theGreatHunt) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype TheGreatHunt = TheGreatHunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

prey :: EnemyMatcher
prey = EnemyInStagingArea <> not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards

-- | You must use resources from 3 different heroes' pools to pay for this card, on its card
-- def.
theGreatHunt :: EventCard TheGreatHunt
theGreatHunt = eventWith TheGreatHunt Cards.theGreatHunt (criteriaL .~ (DuringPhase CombatPhase <> exists prey))

-- | Combat Action: Choose and discard a non-unique enemy in the staging area.
instance RunMessage TheGreatHunt where
  runMessage msg e@(TheGreatHunt attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select prey
      chooseAndDiscard attrs pid (map toTarget enemies)
      pure e
    _ -> TheGreatHunt <$> runMessage msg attrs
