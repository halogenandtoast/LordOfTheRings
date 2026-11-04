module LordOfTheRings.Event.Cards.TheRulingRing (theRulingRing) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Nazgul))

newtype TheRulingRing = TheRulingRing EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Combat Action: Exhaust The One Ring and raise your threat by 1 to choose a non-Nazgûl
-- enemy engaged with you. Until the end of the phase, that enemy cannot attack you.
theRulingRing :: EventCard TheRulingRing
theRulingRing =
  eventWith TheRulingRing Cards.theRulingRing \a ->
    a & criteriaL .~ (DuringPhase CombatPhase <> theOneRingReady a.owner <> exists (choosable a.owner))

choosable :: PlayerId -> EnemyMatcher
choosable pid = EnemyEngagedWith (PlayerWithId pid) <> not_ (EnemyWithTrait Nazgul)

instance RunMessage TheRulingRing where
  runMessage msg e@(TheRulingRing attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      payTheOneRing pid
      enemies <- select (choosable pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        targets enemies \eid -> phaseModifier attrs eid (CannotAttackPlayer pid)
      pure e
    _ -> TheRulingRing <$> runMessage msg attrs
