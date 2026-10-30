module LordOfTheRings.Event.Cards.HornsCry (hornsCry) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype HornsCry = HornsCry EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: All enemies get -1 attack until the end of the phase. Valour Action: Choose a
-- player. Each enemy engaged with that player gets -3 attack until the end of the phase.
hornsCry :: EventCard HornsCry
hornsCry = event HornsCry Cards.hornsCry

instance RunMessage HornsCry where
  runMessage msg e@(HornsCry attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      let weaken :: ReverseQueue n => Int -> EnemyId -> n ()
          weaken n eid = phaseModifier attrs eid (AttackModifier (negate n))
      chooseOrRunOneM pid do
        labeled "hornsCry.allEnemies" $ traverse_ (weaken 1) =<< select AnyEnemy
        valourLabeled pid $ choosePlayerM pid attrs \p -> traverse_ (weaken 3) =<< select (EnemyEngagedWith $ PlayerWithId p)
      pure e
    _ -> HornsCry <$> runMessage msg attrs
