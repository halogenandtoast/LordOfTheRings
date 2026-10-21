module LordOfTheRings.Event.Cards.AstonishingSpeed (astonishingSpeed) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype AstonishingSpeed = AstonishingSpeed EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Until the end of the phase, all Rohan characters get +2 willpower.
astonishingSpeed :: EventCard AstonishingSpeed
astonishingSpeed = event AstonishingSpeed Cards.astonishingSpeed

instance RunMessage AstonishingSpeed where
  runMessage msg e@(AstonishingSpeed attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      rohan <- selectCharacters (HeroWithTrait Rohan) (AllyWithTrait Rohan)
      for_ rohan \c -> phaseModifier attrs c (WillpowerModifier 2)
      pure e
    _ -> AstonishingSpeed <$> runMessage msg attrs
