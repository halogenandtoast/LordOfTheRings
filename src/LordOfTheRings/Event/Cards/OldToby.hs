module LordOfTheRings.Event.Cards.OldToby (oldToby) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Pipe))

newtype OldToby = OldToby EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Pipeweed, on its card def.
oldToby :: EventCard OldToby
oldToby = event OldToby Cards.oldToby

-- | Action: Draw 1 card for each Pipe you control. Heal 1 damage from each hero with a
-- Pipe attachment. (The attachments on your characters are the ones you control.)
instance RunMessage OldToby where
  runMessage msg e@(OldToby attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      pipes <- concatMapM (attachedWithTrait Pipe) =<< charactersControlledBy pid
      unless (null pipes) $ drawCards attrs pid (length pipes)
      heroes <- select (HeroWithAttached Pipe)
      for_ heroes \h -> healDamage attrs h 1
      pure e
    _ -> OldToby <$> runMessage msg attrs
