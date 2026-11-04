module LordOfTheRings.Location.Cards.StagnantPools (stagnantPools) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (exhaustSmeagolToTravel, smeagolTravelModifiers)

newtype StagnantPools = StagnantPools LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

stagnantPools :: LocationCard StagnantPools
stagnantPools = location StagnantPools Cards.stagnantPools

-- | Stagnant Pools gets +1 threat for each mire token on it.
-- Travel: Exhaust Sméagol to travel here.
instance HasModifiersFor StagnantPools where
  getModifiersFor (StagnantPools a) = do
    let mire = countTokens Resource (locationTokens a)
    when (mire > 0) $ modifySelf a [ThreatModifier mire]
    smeagolTravelModifiers a

-- | Mire 5.
-- Forced: When Stagnant Pools is discarded by the Mire keyword, raise each player's threat
-- by 5.
instance HasAbilities StagnantPools where
  getAbilities (StagnantPools a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

instance RunMessage StagnantPools where
  runMessage msg l@(StagnantPools a) = case msg of
    TravelTo lid | lid == a.id -> do
      exhaustSmeagolToTravel
      StagnantPools <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid 5
      pure l
    _ -> StagnantPools <$> runMessage msg a
