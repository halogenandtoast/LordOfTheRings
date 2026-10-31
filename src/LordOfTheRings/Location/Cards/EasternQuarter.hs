module LordOfTheRings.Location.Cards.EasternQuarter (easternQuarter) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers (returnToStaging)

newtype EasternQuarter = EasternQuarter LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easternQuarter :: LocationCard EasternQuarter
easternQuarter = location EasternQuarter Cards.easternQuarter

-- | While a player controls Eastern Quarter it gains: "Forced: After the 'when revealed'
-- effect of a treachery is canceled, return Eastern Quarter to the staging area."
instance HasAbilities EasternQuarter where
  getAbilities (EasternQuarter a) =
    [mkAbility a 1 $ forced $ TreacheryWhenRevealedCanceledWindow After | InPlayArea _ <- [a.placement]]

-- | Forced: When you take control of Eastern Quarter, discard each event card in your hand.
instance RunMessage EasternQuarter where
  runMessage msg l@(EasternQuarter a) = case msg of
    PlaceLocation lid (InPlayArea pid) | lid == a.id && a.placement /= InPlayArea pid -> do
      discardEventsFromHand pid
      EasternQuarter <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnToStaging a.id
      pure l
    _ -> EasternQuarter <$> runMessage msg a
