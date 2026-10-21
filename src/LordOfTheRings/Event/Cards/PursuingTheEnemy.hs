module LordOfTheRings.Event.Cards.PursuingTheEnemy (pursuingTheEnemy) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype PursuingTheEnemy = PursuingTheEnemy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Return a Silvan ally you control to your hand to choose a player. Deal 1
-- damage to each enemy engaged with that player.
pursuingTheEnemy :: EventCard PursuingTheEnemy
pursuingTheEnemy =
  eventWith PursuingTheEnemy Cards.pursuingTheEnemy \a -> a & criteriaL .~ controlsAlly a.owner (AllyWithTrait Silvan)

instance RunMessage PursuingTheEnemy where
  runMessage msg e@(PursuingTheEnemy attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      returnAllyToHand pid attrs (AllyWithTrait Silvan)
      choosePlayerM pid attrs \p -> do
        enemies <- select (EnemyEngagedWith $ PlayerWithId p)
        for_ enemies \en -> dealDamage attrs en 1
      pure e
    _ -> PursuingTheEnemy <$> runMessage msg attrs
