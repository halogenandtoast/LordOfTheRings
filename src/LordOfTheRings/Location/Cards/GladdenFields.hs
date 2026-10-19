module LordOfTheRings.Location.Cards.GladdenFields (gladdenFields) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype GladdenFields = GladdenFields LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gladdenFields :: LocationCard GladdenFields
gladdenFields = location GladdenFields Cards.gladdenFields

-- | Forced: While Gladden Fields is the active location, each player must raise his threat
-- by an additional point during the refresh phase.
instance HasAbilities GladdenFields where
  getAbilities (GladdenFields a) =
    [restricted a 1 (LocationExists $ LocationWithId a.id <> IsActiveLocation) $ forced $ PhaseBeginsWindow After RefreshPhase]

instance RunMessage GladdenFields where
  runMessage msg l@(GladdenFields a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      order <- getPlayerOrder
      pushAll [RaiseThreat p 1 | p <- order]
      pure l
    _ -> GladdenFields <$> runMessage msg a
