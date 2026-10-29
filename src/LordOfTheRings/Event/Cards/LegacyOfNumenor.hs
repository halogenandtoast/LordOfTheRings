module LordOfTheRings.Event.Cards.LegacyOfNumenor (legacyOfNumenor) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype LegacyOfNumenor = LegacyOfNumenor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 4, on its card def. Action: Add 1 resource to each hero's resource pool.
legacyOfNumenor :: EventCard LegacyOfNumenor
legacyOfNumenor = event LegacyOfNumenor Cards.legacyOfNumenor

instance RunMessage LegacyOfNumenor where
  runMessage msg e@(LegacyOfNumenor attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      heroes <- select AnyHero
      for_ heroes \h -> placeResources attrs h 1
      pure e
    _ -> LegacyOfNumenor <$> runMessage msg attrs
