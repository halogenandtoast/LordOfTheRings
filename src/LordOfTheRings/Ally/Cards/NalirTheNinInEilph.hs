module LordOfTheRings.Ally.Cards.NalirTheNinInEilph (nalirTheNinInEilph) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (placementController)

newtype NalirTheNinInEilph = NalirTheNinInEilph AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
nalirTheNinInEilph :: AllyCard NalirTheNinInEilph
nalirTheNinInEilph = ally NalirTheNinInEilph Cards.nalirTheNinInEilph

-- | Forced: At the beginning of the refresh phase, raise your threat by 1 for each player
-- in the game.
instance HasAbilities NalirTheNinInEilph where
  getAbilities (NalirTheNinInEilph a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After RefreshPhase]

-- | If Nalir leaves play, the players lose the game.
instance RunMessage NalirTheNinInEilph where
  runMessage msg s@(NalirTheNinInEilph a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      for_ (placementController a.placement) \pid -> encounterRaisesThreat pid players
      pure s
    Discard (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> NalirTheNinInEilph <$> runMessage msg a
