module LordOfTheRings.Event.Cards.PillarsOfTheKings (pillarsOfTheKings) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype PillarsOfTheKings = PillarsOfTheKings EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Set your threat to 40. If this effect raised your threat, draw a card (draw 4
-- cards instead if it raised your threat by 10 or more).
pillarsOfTheKings :: EventCard PillarsOfTheKings
pillarsOfTheKings = event PillarsOfTheKings Cards.pillarsOfTheKings

-- | While your threat is less than 40, reduce the cost to play Pillars of the Kings by 4.
instance HasModifiersFor PillarsOfTheKings where
  getModifiersFor (PillarsOfTheKings a) = do
    threat <- (.threat) <$> getPlayer a.owner
    when (threat < 40) $ modifySelf a [CostModifier (-4)]

instance RunMessage PillarsOfTheKings where
  runMessage msg e@(PillarsOfTheKings attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      threat <- (.threat) <$> getPlayer pid
      if threat < 40
        then do
          raiseThreat pid (40 - threat)
          drawCards pid $ if 40 - threat >= 10 then 4 else 1
        else lowerThreat pid (threat - 40)
      pure e
    _ -> PillarsOfTheKings <$> runMessage msg attrs
