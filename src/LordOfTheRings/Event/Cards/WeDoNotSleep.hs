module LordOfTheRings.Event.Cards.WeDoNotSleep (weDoNotSleep) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype WeDoNotSleep = WeDoNotSleep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Until the end of the phase, Rohan characters do not exhaust to commit to
-- quests.
weDoNotSleep :: EventCard WeDoNotSleep
weDoNotSleep = event WeDoNotSleep Cards.weDoNotSleep

instance RunMessage WeDoNotSleep where
  runMessage msg e@(WeDoNotSleep attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      phaseModifier attrs GameTarget (TraitDoesNotExhaustToCommit Rohan)
      pure e
    _ -> WeDoNotSleep <$> runMessage msg attrs
