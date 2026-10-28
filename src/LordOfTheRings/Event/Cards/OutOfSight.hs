module LordOfTheRings.Event.Cards.OutOfSight (outOfSight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype OutOfSight = OutOfSight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 3, on its card def.
-- Action: Enemies engaged with you cannot attack you this phase, those that engage you
-- later in the phase too.
outOfSight :: EventCard OutOfSight
outOfSight = event OutOfSight Cards.outOfSight

instance RunMessage OutOfSight where
  runMessage msg e@(OutOfSight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      phaseModifier attrs (PlayerTarget pid) EngagedEnemiesCannotAttack
      pure e
    _ -> OutOfSight <$> runMessage msg attrs
