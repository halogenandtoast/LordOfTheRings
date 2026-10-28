module LordOfTheRings.Event.Cards.TheHammerStroke (theHammerStroke) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))

newtype TheHammerStroke = TheHammerStroke EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Encounter Action: Engage each enemy in play.
theHammerStroke :: EventCard TheHammerStroke
theHammerStroke =
  eventWith TheHammerStroke Cards.theHammerStroke \a ->
    a & criteriaL .~ DuringPhase EncounterPhase <> exists (not_ $ EnemyEngagedWith $ PlayerWithId a.owner)

instance RunMessage TheHammerStroke where
  runMessage msg e@(TheHammerStroke attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (not_ $ EnemyEngagedWith $ PlayerWithId pid)
      for_ enemies (engageEnemy pid)
      pure e
    _ -> TheHammerStroke <$> runMessage msg attrs
