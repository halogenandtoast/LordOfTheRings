module LordOfTheRings.Location.Cards.WesternQuarter (westernQuarter) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers (returnToStaging)

newtype WesternQuarter = WesternQuarter LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

westernQuarter :: LocationCard WesternQuarter
westernQuarter = location WesternQuarter Cards.westernQuarter

-- | While a player controls Western Quarter it gains: "Forced: After the 'when revealed'
-- effect of a treachery is canceled, return Western Quarter to the staging area."
instance HasAbilities WesternQuarter where
  getAbilities (WesternQuarter a) =
    [mkAbility a 1 $ forced $ TreacheryWhenRevealedCanceledWindow After | InPlayArea _ <- [a.placement]]

-- | Forced: When you take control of Western Quarter, discard the highest cost attachment
-- you control: he picks among those tied.
instance RunMessage WesternQuarter where
  runMessage msg l@(WesternQuarter a) = case msg of
    PlaceLocation lid (InPlayArea pid) | lid == a.id && a.placement /= InPlayArea pid -> do
      discardHighestCostAttachment a pid
      WesternQuarter <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnToStaging a.id
      pure l
    _ -> WesternQuarter <$> runMessage msg a
