module LordOfTheRings.Location.Cards.SlopesOfHithaeglir (slopesOfHithaeglir) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (damageEachActiveLocation)

newtype SlopesOfHithaeglir = SlopesOfHithaeglir LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slopesOfHithaeglir :: LocationCard SlopesOfHithaeglir
slopesOfHithaeglir = location SlopesOfHithaeglir Cards.slopesOfHithaeglir

instance HasAbilities SlopesOfHithaeglir where
  getAbilities (SlopesOfHithaeglir a) =
    [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After, mkAbility a 2 $ forced $ ExploredWindow When a.id]

instance RunMessage SlopesOfHithaeglir where
  runMessage msg l@(SlopesOfHithaeglir a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      damageEachActiveLocation a 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer (\pid -> encounterRaisesThreat pid a.damage)
      pure l
    _ -> SlopesOfHithaeglir <$> runMessage msg a
