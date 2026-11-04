module LordOfTheRings.Location.Cards.AccursedForest (accursedForest) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype AccursedForest = AccursedForest LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

accursedForest :: LocationCard AccursedForest
accursedForest = location AccursedForest Cards.accursedForest

-- | While Accursed Forest is in the staging area, 'when revealed' effects cannot be canceled.
instance HasModifiersFor AccursedForest where
  getModifiersFor (AccursedForest a) = when (a.placement == StagingArea) $ modified a GameTarget [WhenRevealedCannotBeCanceled]

-- | Travel: Reveal the top card of the encounter deck.
instance RunMessage AccursedForest where
  runMessage msg (AccursedForest a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      AccursedForest <$> runMessage msg a
    _ -> AccursedForest <$> runMessage msg a
