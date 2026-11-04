module LordOfTheRings.Event.Cards.TheFreePeoples (theFreePeoples) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheFreePeoples = TheFreePeoples EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if the characters you control have a total of at least 9 different Traits
-- between them.
theFreePeoples :: EventCard TheFreePeoples
theFreePeoples = eventWith TheFreePeoples Cards.theFreePeoples \a -> a & criteriaL .~ DifferentTraitsAtLeast a.owner 9

-- | Action: Ready each character in play. Until the end of the phase, each character you
-- control gets +1 willpower.
instance RunMessage TheFreePeoples where
  runMessage msg e@(TheFreePeoples attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      traverse_ ready =<< selectCharacters AnyHero AnyAlly
      characters <- charactersControlledBy pid
      for_ characters \c -> phaseModifier attrs c (WillpowerModifier 1)
      pure e
    _ -> TheFreePeoples <$> runMessage msg attrs
