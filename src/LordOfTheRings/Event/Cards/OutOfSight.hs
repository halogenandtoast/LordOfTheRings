module LordOfTheRings.Event.Cards.OutOfSight (outOfSight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype OutOfSight = OutOfSight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 3, on its card def.
-- Action: Enemies engaged with you cannot attack you this phase: those engaged with you as
-- it is played, a lasting effect of a player card being worked out once (FAQ 1.55). (FAQ
-- 1.9 errata; as printed, they cannot attack this phase.)
outOfSight :: EventCard OutOfSight
outOfSight = event OutOfSight Cards.outOfSight

instance RunMessage OutOfSight where
  runMessage msg e@(OutOfSight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      errata <- playsWithErrata pid
      for_ enemies \eid -> phaseModifier attrs eid if errata then CannotAttackPlayer pid else CannotAttack
      pure e
    _ -> OutOfSight <$> runMessage msg attrs
