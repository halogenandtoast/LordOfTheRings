module LordOfTheRings.Event.Cards.SmokeRings (smokeRings) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Pipe))

newtype SmokeRings = SmokeRings EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

smokeRings :: EventCard SmokeRings
smokeRings = event SmokeRings Cards.smokeRings

-- | Action: Reduce your threat by 1 for each Pipe you control. Each hero with a Pipe
-- attachment gets +1 [willpower] until the end of the phase. The attachments on your
-- characters are the ones you control.
instance RunMessage SmokeRings where
  runMessage msg e@(SmokeRings attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      pipes <- length <$> (concatMapM (attachedWithTrait Pipe) =<< charactersControlledBy pid)
      when (pipes > 0) $ lowerThreatBy attrs pid pipes
      heroes <- select (HeroWithAttached Pipe)
      for_ heroes \h -> phaseModifier attrs h (WillpowerModifier 1)
      pure e
    _ -> SmokeRings <$> runMessage msg attrs
