module LordOfTheRings.Event.Cards.AgainstTheShadow (againstTheShadow) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype AgainstTheShadow = AgainstTheShadow EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if each hero you control has the printed spirit resource icon.
-- Action: Until the end of the phase, spirit characters use their willpower instead of
-- defense.
againstTheShadow :: EventCard AgainstTheShadow
againstTheShadow = eventWith AgainstTheShadow Cards.againstTheShadow \a -> a & criteriaL .~ eachHeroHasPrintedIcon a.owner Spirit

instance RunMessage AgainstTheShadow where
  runMessage msg e@(AgainstTheShadow attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      -- a player card's lasting effect is fixed as it resolves (FAQ 1.55): the spirit
      -- characters then in play
      characters <- selectCharacters (HeroWithSphere Spirit) (AllyWithSphere Spirit)
      for_ characters \c -> phaseModifier attrs c WillpowerForDefense
      pure e
    _ -> AgainstTheShadow <$> runMessage msg attrs
