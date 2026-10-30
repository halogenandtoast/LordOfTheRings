module LordOfTheRings.Event.Cards.HourOfWrath (hourOfWrath) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype HourOfWrath = HourOfWrath EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a hero. Until the end of the phase, the chosen hero does not exhaust to
-- attack or defend.
-- Valour Action: Choose a player. Until the end of the phase, each of that player's heroes
-- do not exhaust to attack or defend.
hourOfWrath :: EventCard HourOfWrath
hourOfWrath = event HourOfWrath Cards.hourOfWrath

unwearied :: ReverseQueue m => EventAttrs -> HeroId -> m ()
unwearied attrs h = phaseModifiers attrs h [DoesNotExhaustToAttack, DoesNotExhaustToDefend]

instance RunMessage HourOfWrath where
  runMessage msg e@(HourOfWrath attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select AnyHero
      chooseOrRunOneM pid do
        questionLabeled "hourOfWrath.chooseOne"
        causedBy attrs
        targets heroes (unwearied attrs)
        valourLabeled pid $ choosePlayerM pid attrs \p -> traverse_ (unwearied attrs) =<< select (HeroControlledBy $ PlayerWithId p)
      pure e
    _ -> HourOfWrath <$> runMessage msg attrs
