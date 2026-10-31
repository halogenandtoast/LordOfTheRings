module LordOfTheRings.Location.Cards.RavenStronghold (ravenStronghold) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (addRavenCardToStaging, revealRavenCard)

newtype RavenStronghold = RavenStronghold LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenStronghold :: LocationCard RavenStronghold
ravenStronghold = location RavenStronghold Cards.ravenStronghold

-- | Time 3. Forced: After the last time counter is removed from this location, each player
-- reveals the top card of the Raven deck and adds it to the staging area.
-- Forced: When this location is explored, add the top card of the Raven deck to the
-- staging area.
instance HasAbilities RavenStronghold where
  getAbilities (RavenStronghold a) =
    [ mkAbility a 1 $ forced $ lastTimeCounterRemoved a
    , mkAbility a 2 $ forced $ ExploredWindow When a.id
    ]

instance RunMessage RavenStronghold where
  runMessage msg l@(RavenStronghold a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \_ -> revealRavenCard
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      addRavenCardToStaging
      pure l
    _ -> RavenStronghold <$> runMessage msg a
