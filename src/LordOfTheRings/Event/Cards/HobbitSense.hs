module LordOfTheRings.Event.Cards.HobbitSense (hobbitSense) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype HobbitSense = HobbitSense EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if each of your heroes is a Hobbit.
-- Combat Action: Enemies engaged with you do not attack this round. You cannot declare
-- attacks this round.
hobbitSense :: EventCard HobbitSense
hobbitSense =
  eventWith HobbitSense Cards.hobbitSense \a ->
    a
      & criteriaL
      .~ DuringPhase CombatPhase
      <> NotCriterion (exists $ HeroControlledBy (PlayerWithId a.owner) <> NotHero (HeroWithTrait Hobbit))

instance RunMessage HobbitSense where
  runMessage msg e@(HobbitSense attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      roundModifiers attrs (PlayerTarget pid) [EngagedEnemiesCannotAttack, CannotAttack]
      pure e
    _ -> HobbitSense <$> runMessage msg attrs
