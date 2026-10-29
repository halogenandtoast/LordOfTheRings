module LordOfTheRings.Event.Cards.LightTheBeacons (lightTheBeacons) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype LightTheBeacons = LightTheBeacons EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: All characters get +2 defense and do not exhaust to defend until the end of
-- the round.
lightTheBeacons :: EventCard LightTheBeacons
lightTheBeacons = event LightTheBeacons Cards.lightTheBeacons

instance RunMessage LightTheBeacons where
  runMessage msg e@(LightTheBeacons attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      -- the characters in play as it resolves: a player card's lasting effect is not
      -- recalculated (FAQ 1.55)
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifiers attrs c [DefenseModifier 2, DoesNotExhaustToDefend]
      pure e
    _ -> LightTheBeacons <$> runMessage msg attrs
