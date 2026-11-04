module LordOfTheRings.Event.Cards.TheRingOfPower (theRingOfPower) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheRingOfPower = TheRingOfPower EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust The One Ring and raise your threat by 1 to ready the hero with The One
-- Ring attached. That hero gets +1 willpower, +1 attack and +1 defense until the end of the
-- phase.
theRingOfPower :: EventCard TheRingOfPower
theRingOfPower = eventWith TheRingOfPower Cards.theRingOfPower \a -> a & criteriaL .~ theOneRingReady a.owner

instance RunMessage TheRingOfPower where
  runMessage msg e@(TheRingOfPower attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      payTheOneRing pid
      heroes <- select heroWithTheOneRing
      for_ heroes \h -> do
        ready h
        phaseModifiers attrs h [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure e
    _ -> TheRingOfPower <$> runMessage msg attrs
