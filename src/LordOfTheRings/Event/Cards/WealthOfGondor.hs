module LordOfTheRings.Event.Cards.WealthOfGondor (wealthOfGondor) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype WealthOfGondor = WealthOfGondor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a Gondor hero. Add 1 resource to that hero's resource pool.
wealthOfGondor :: EventCard WealthOfGondor
wealthOfGondor = eventWith WealthOfGondor Cards.wealthOfGondor (criteriaL .~ exists (HeroWithTrait Gondor))

instance RunMessage WealthOfGondor where
  runMessage msg e@(WealthOfGondor attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroWithTrait Gondor)
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroForResource"
        causedBy attrs
        targets heroes \h -> placeResources attrs h 1
      pure e
    _ -> WealthOfGondor <$> runMessage msg attrs
