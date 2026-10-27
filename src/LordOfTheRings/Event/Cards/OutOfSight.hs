module LordOfTheRings.Event.Cards.OutOfSight (outOfSight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype OutOfSight = OutOfSight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 3, on its card def.
-- Action: Enemies engaged with you cannot attack you this phase. Those engaged with you
-- as it is played.
outOfSight :: EventCard OutOfSight
outOfSight = event OutOfSight Cards.outOfSight

instance RunMessage OutOfSight where
  runMessage msg e@(OutOfSight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
      for_ engaged \eid -> phaseModifier attrs eid (CannotAttackPlayer pid)
      pure e
    _ -> OutOfSight <$> runMessage msg attrs
