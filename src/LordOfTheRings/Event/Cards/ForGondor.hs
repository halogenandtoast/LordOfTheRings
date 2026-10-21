module LordOfTheRings.Event.Cards.ForGondor (forGondor) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype ForGondor = ForGondor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Until the end of the phase, all characters get +1 attack. All Gondor
-- characters also get +1 defense until the end of the phase.
forGondor :: EventCard ForGondor
forGondor = event ForGondor Cards.forGondor

instance RunMessage ForGondor where
  runMessage msg e@(ForGondor attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      characters <- selectCharacters AnyHero AnyAlly
      gondor <- selectCharacters (HeroWithTrait Gondor) (AllyWithTrait Gondor)
      for_ characters \c -> phaseModifiers attrs c (AttackModifier 1 : [DefenseModifier 1 | c `elem` gondor])
      pure e
    _ -> ForGondor <$> runMessage msg attrs
