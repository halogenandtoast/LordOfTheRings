module LordOfTheRings.Event.Cards.KnifeWork (knifeWork) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype KnifeWork = KnifeWork EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

knifeWork :: EventCard KnifeWork
knifeWork = event KnifeWork Cards.knifeWork

-- | Action: Choose a player. Each enemy engaged with that player gets -1 defense until the
-- end of the phase. Then, the players as a group may spend 2 lore resources to have the
-- chosen player draw 1 card each time he attacks and destroys an enemy this phase.
instance RunMessage KnifeWork where
  runMessage msg e@(KnifeWork attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> do
        enemies <- select (EnemyEngagedWith $ PlayerWithId p)
        for_ enemies \eid -> phaseModifier attrs eid (DefenseModifier (-1))
        groupMaySpend pid attrs Lore 2 $ phaseModifier attrs (PlayerTarget p) DrawsOnDestroyingEnemy
      pure e
    _ -> KnifeWork <$> runMessage msg attrs
