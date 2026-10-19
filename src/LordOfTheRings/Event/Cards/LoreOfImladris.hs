module LordOfTheRings.Event.Cards.LoreOfImladris (loreOfImladris) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype LoreOfImladris = LoreOfImladris EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a character. Heal all damage from that character.
loreOfImladris :: EventCard LoreOfImladris
loreOfImladris = event LoreOfImladris Cards.loreOfImladris

instance RunMessage LoreOfImladris where
  runMessage msg e@(LoreOfImladris attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      choices <- for damaged \c -> targetLabel c . pure . HealDamage (toSource attrs) c <$> getDamage c
      unless (null choices) $ push $ questionLabelWith "Heal all damage from a character" (Just $ toTarget attrs) [] pid choices
      pure e
    _ -> LoreOfImladris <$> runMessage msg attrs
