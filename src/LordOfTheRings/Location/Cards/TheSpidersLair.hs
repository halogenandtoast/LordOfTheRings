module LordOfTheRings.Location.Cards.TheSpidersLair (theSpidersLair) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (getShelob)

newtype TheSpidersLair = TheSpidersLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSpidersLair :: LocationCard TheSpidersLair
theSpidersLair = location TheSpidersLair Cards.theSpidersLair

-- | Forced: When The Spider's Lair is explored, remove 1 resource from Shelob.
-- Forced: After The Spider's Lair becomes the active location, Shelob makes an attack
-- against the first player.
instance HasAbilities TheSpidersLair where
  getAbilities (TheSpidersLair a) =
    [ mkAbility a 1 $ forced $ ExploredWindow When a.id
    , mkAbility a 2 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)
    ]

instance RunMessage TheSpidersLair where
  runMessage msg l@(TheSpidersLair a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      getShelob >>= traverse_ \e -> removeTokens a e Resource 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      lead <- getLead
      getShelob >>= traverse_ (immediateAttack lead)
      pure l
    _ -> TheSpidersLair <$> runMessage msg a
