module LordOfTheRings.Event.Cards.RecklessAttack (recklessAttack) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype RecklessAttack = RecklessAttack EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 2, on its card def. Action: Each player deals 2 damage to an enemy engaged with
-- him.
recklessAttack :: EventCard RecklessAttack
recklessAttack = event RecklessAttack Cards.recklessAttack

instance RunMessage RecklessAttack where
  runMessage msg e@(RecklessAttack attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> do
        enemies <- select (EnemyEngagedWith $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "recklessAttack.chooseEnemy"
          causedBy attrs
          targets enemies \eid -> dealDamage attrs eid 2
      pure e
    _ -> RecklessAttack <$> runMessage msg attrs
