module LordOfTheRings.Location.Cards.RavenWarCamp (ravenWarCamp) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (putRavenCardIntoPlayEngaged)

newtype RavenWarCamp = RavenWarCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenWarCamp :: LocationCard RavenWarCamp
ravenWarCamp = location RavenWarCamp Cards.ravenWarCamp

-- | Forced: After the last time counter is removed from this location, starting with the
-- first player, each player draws the top card of the Raven deck and puts it into play
-- engaged with him.
instance HasAbilities RavenWarCamp where
  getAbilities (RavenWarCamp a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

instance RunMessage RavenWarCamp where
  runMessage msg l@(RavenWarCamp a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer putRavenCardIntoPlayEngaged
      pure l
    _ -> RavenWarCamp <$> runMessage msg a
