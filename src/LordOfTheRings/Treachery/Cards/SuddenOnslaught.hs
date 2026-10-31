module LordOfTheRings.Treachery.Cards.SuddenOnslaught (suddenOnslaught) where

import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, takeHiddenCard, turnFaceup, turnHiddenCardsFaceup)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuddenOnslaught = SuddenOnslaught TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

suddenOnslaught :: TreacheryCard SuddenOnslaught
suddenOnslaught = treachery SuddenOnslaught Cards.suddenOnslaught

-- | When Revealed: Each player turns each hidden card in his play area faceup. Each player
-- then takes hidden cards equal to the number of hidden cards he just turned faceup.
-- Shadow: Take 1 hidden card. Then, turn 1 hidden card in your play area faceup: the first
-- taken, as they are facedown and any is as good as another.
instance RunMessage SuddenOnslaught where
  runMessage msg t@(SuddenOnslaught attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      counts <- for players \pid -> (pid,) . length <$> getHiddenCards pid
      for_ players (void . turnHiddenCardsFaceup)
      for_ counts \(pid, n) -> replicateM_ n (takeHiddenCard pid)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      takeHiddenCard pid
      doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      traverse_ (turnFaceup pid) . take 1 =<< getHiddenCards pid
      pure t
    _ -> SuddenOnslaught <$> runMessage msg attrs
