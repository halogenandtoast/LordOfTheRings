module LordOfTheRings.Location.Cards.AncestralClearing (ancestralClearing) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype AncestralClearing = AncestralClearing LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancestralClearing :: LocationCard AncestralClearing
ancestralClearing = location AncestralClearing Cards.ancestralClearing

-- | The cost to play each player card is increased by 1.
-- Travel: Exhaust a hero to travel here.
instance HasModifiersFor AncestralClearing where
  getModifiersFor (AncestralClearing a) = do
    modified a GameTarget [IncreaseCost AnyCard 1]
    stuck <- selectNone ReadyHero
    when stuck $ modified a a [CannotBeTraveledTo]

instance RunMessage AncestralClearing where
  runMessage msg l@(AncestralClearing a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      heroes <- select ReadyHero
      chooseOneM lead do
        questionLabeled "ancestralClearing.exhaustHero"
        causedBy a
        targets heroes exhaust
      pure l
    _ -> AncestralClearing <$> runMessage msg a
