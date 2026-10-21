module LordOfTheRings.Event.Cards.CampfireTales (campfireTales) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype CampfireTales = CampfireTales EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Each player draws 1 card.
campfireTales :: EventCard CampfireTales
campfireTales = event CampfireTales Cards.campfireTales

instance RunMessage CampfireTales where
  runMessage msg e@(CampfireTales attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> drawCards p 1
      pure e
    _ -> CampfireTales <$> runMessage msg attrs
