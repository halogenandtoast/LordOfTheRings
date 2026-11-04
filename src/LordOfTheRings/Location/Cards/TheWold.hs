module LordOfTheRings.Location.Cards.TheWold (theWold) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype TheWold = TheWold LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWold :: LocationCard TheWold
theWold = location TheWold Cards.theWold

-- | While The Wold is the active location, it gains: "Response: After The Wold is
-- explored, each player draws 1 card."
instance HasAbilities TheWold where
  getAbilities (TheWold a) = [mkAbility a 1 $ response $ ExploredWindow After a.id | a.placement == ActiveLocation]

-- | Travel: Increase the pursuit value by 1.
instance RunMessage TheWold where
  runMessage msg l@(TheWold a) = case msg of
    TravelTo lid | lid == a.id -> do
      increasePursuit 1
      TheWold <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> drawCards a p 1
      pure l
    _ -> TheWold <$> runMessage msg a
