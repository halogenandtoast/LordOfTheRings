module LordOfTheRings.Event.Cards.FrodosIntuition (frodosIntuition) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype FrodosIntuition = FrodosIntuition EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

frodosIntuition :: EventCard FrodosIntuition
frodosIntuition = event FrodosIntuition Cards.frodosIntuition

-- | Action: Each hero you control gets +1 [willpower] until the end of the round. Draw 1
-- card for each Hobbit hero you control.
instance RunMessage FrodosIntuition where
  runMessage msg e@(FrodosIntuition attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> roundModifier attrs h (WillpowerModifier 1)
      hobbits <- length <$> select (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Hobbit)
      when (hobbits > 0) $ drawCards attrs pid hobbits
      pure e
    _ -> FrodosIntuition <$> runMessage msg attrs
