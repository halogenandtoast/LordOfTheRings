module LordOfTheRings.Treachery.Cards.RuinsOfAgesPast (ruinsOfAgesPast) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RuinsOfAgesPast = RuinsOfAgesPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinsOfAgesPast :: TreacheryCard RuinsOfAgesPast
ruinsOfAgesPast = treachery RuinsOfAgesPast Cards.ruinsOfAgesPast

-- | Doomed 2.
-- When Revealed: Add 1 copy of Lost Island from the top of the Uncharted deck to the
-- staging area. Remove all progress from each Uncharted location in the staging area and
-- shuffle them so that the players do not know which one is which.
instance RunMessage RuinsOfAgesPast where
  runMessage msg t@(RuinsOfAgesPast attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addFromUnchartedDeck 1
      shuffleUnchartedLocations
      pure t
    _ -> RuinsOfAgesPast <$> runMessage msg attrs
