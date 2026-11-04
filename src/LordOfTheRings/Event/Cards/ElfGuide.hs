module LordOfTheRings.Event.Cards.ElfGuide (elfGuide) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype ElfGuide = ElfGuide EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Return a Silvan ally you control to your hand to choose a hero. Add 1 resource
-- to that hero's pool.
elfGuide :: EventCard ElfGuide
elfGuide =
  eventWith ElfGuide Cards.elfGuide \a -> a & criteriaL .~ (controlsAlly a.owner (AllyWithTrait Silvan) <> exists AnyHero)

instance RunMessage ElfGuide where
  runMessage msg e@(ElfGuide attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      returnAllyToHand pid attrs (AllyWithTrait Silvan)
      heroes <- select AnyHero
      chooseOrRunOneM pid do
        questionLabeled "elfGuide.hero"
        causedBy attrs
        targets heroes \h -> placeResources attrs h 1
      pure e
    _ -> ElfGuide <$> runMessage msg attrs
