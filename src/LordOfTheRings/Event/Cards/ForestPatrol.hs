module LordOfTheRings.Event.Cards.ForestPatrol (forestPatrol) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger, Trap))

newtype ForestPatrol = ForestPatrol EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control at least 1 Ranger character.
-- Action: Deal 3 damage to an enemy with at least 1 Trap card attached to it.
forestPatrol :: EventCard ForestPatrol
forestPatrol =
  eventWith ForestPatrol Cards.forestPatrol \a ->
    a
      & criteriaL
      .~ CharactersAtLeast 1 (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Ranger) (AllyControlledBy (PlayerWithId a.owner) <> AllyWithTrait Ranger)
      <> exists (EnemyWithAttached Trap)

instance RunMessage ForestPatrol where
  runMessage msg e@(ForestPatrol attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (EnemyWithAttached Trap)
      chooseOrRunOneM pid do
        questionLabeled "forestPatrol.damage"
        causedBy attrs
        targets enemies \eid -> dealDamage attrs eid 3
      pure e
    _ -> ForestPatrol <$> runMessage msg attrs
