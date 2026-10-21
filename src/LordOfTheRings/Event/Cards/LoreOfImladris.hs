module LordOfTheRings.Event.Cards.LoreOfImladris (loreOfImladris) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype LoreOfImladris = LoreOfImladris EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a character. Heal all damage from that character.
loreOfImladris :: EventCard LoreOfImladris
loreOfImladris = event LoreOfImladris Cards.loreOfImladris

instance RunMessage LoreOfImladris where
  runMessage msg e@(LoreOfImladris attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "loreOfImladris.heal"
        causedBy attrs
        targets damaged $ healAllDamage attrs
      pure e
    _ -> LoreOfImladris <$> runMessage msg attrs
