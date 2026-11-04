module LordOfTheRings.Event.Cards.TheShirefolk (theShirefolk) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype TheShirefolk = TheShirefolk EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if each of your heroes is a Hobbit.
theShirefolk :: EventCard TheShirefolk
theShirefolk =
  eventWith TheShirefolk Cards.theShirefolk \a ->
    a & criteriaL .~ NotCriterion (exists $ HeroControlledBy (PlayerWithId a.owner) <> NotHero (HeroWithTrait Hobbit))

-- | Action: Reduce your threat by 4.
instance RunMessage TheShirefolk where
  runMessage msg e@(TheShirefolk attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      lowerThreatBy attrs pid 4
      pure e
    _ -> TheShirefolk <$> runMessage msg attrs
