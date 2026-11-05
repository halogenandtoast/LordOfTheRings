module LordOfTheRings.Location.Cards.SecretTunnel (secretTunnel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (burgled)

newtype SecretTunnel = SecretTunnel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

secretTunnel :: LocationCard SecretTunnel
secretTunnel = location SecretTunnel Cards.secretTunnel

instance HasModifiersFor SecretTunnel where
  getModifiersFor (SecretTunnel a) = do
    bilbo <- getBilbo
    readyBilbo <- selectAny (HeroWithTitle "Bilbo Baggins" <> ReadyHero)
    resources <- getBagginsResources
    unless (isJust bilbo && readyBilbo && resources >= 1) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage SecretTunnel where
  runMessage msg l@(SecretTunnel a) = case msg of
    (burgled -> Just (_, False)) -> do
      eachPlayer \pid -> encounterRaisesThreat pid 2
      pure l
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      bilbo <- getBilbo
      for_ bilbo \b -> do
        exhaust b
        spendResourcesFrom lead b 1
      SecretTunnel <$> runMessage msg a
    _ -> SecretTunnel <$> runMessage msg a
