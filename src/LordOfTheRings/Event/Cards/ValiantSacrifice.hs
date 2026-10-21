module LordOfTheRings.Event.Cards.ValiantSacrifice (valiantSacrifice) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ValiantSacrifice = ValiantSacrifice EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After an ally card leaves play, that card's controller draws 2 cards.
valiantSacrifice :: EventCard ValiantSacrifice
valiantSacrifice = eventWith ValiantSacrifice Cards.valiantSacrifice (windowL ?~ AllyLeftPlayWindow After)

instance RunMessage ValiantSacrifice where
  runMessage msg e@(ValiantSacrifice attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (headMay [controller | Window _ (LeftPlay (AllyTarget _) controller _) <- windows]) \controller ->
        drawCards controller 2
      pure e
    _ -> ValiantSacrifice <$> runMessage msg attrs
