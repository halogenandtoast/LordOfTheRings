module LordOfTheRings.Event.Cards.WatersOfNimrodel (watersOfNimrodel) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype WatersOfNimrodel = WatersOfNimrodel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 3, on its card def. Action: Heal all damage on each character in play.
watersOfNimrodel :: EventCard WatersOfNimrodel
watersOfNimrodel = event WatersOfNimrodel Cards.watersOfNimrodel

instance RunMessage WatersOfNimrodel where
  runMessage msg e@(WatersOfNimrodel attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters (healAllDamage attrs)
      pure e
    _ -> WatersOfNimrodel <$> runMessage msg attrs
