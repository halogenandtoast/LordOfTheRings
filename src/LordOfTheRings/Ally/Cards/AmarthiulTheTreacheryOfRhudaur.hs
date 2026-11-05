module LordOfTheRings.Ally.Cards.AmarthiulTheTreacheryOfRhudaur (amarthiulTheTreacheryOfRhudaur) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (placementController)

newtype AmarthiulTheTreacheryOfRhudaur = AmarthiulTheTreacheryOfRhudaur AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
amarthiulTheTreacheryOfRhudaur :: AllyCard AmarthiulTheTreacheryOfRhudaur
amarthiulTheTreacheryOfRhudaur = ally AmarthiulTheTreacheryOfRhudaur Cards.amarthiulTheTreacheryOfRhudaur

-- | Response: After an enemy engages a player, give control of Amarthiúl to that player.
instance HasAbilities AmarthiulTheTreacheryOfRhudaur where
  getAbilities (AmarthiulTheTreacheryOfRhudaur a) =
    [mkAbility a 1 $ response $ OtherPlayerEngagedEnemyWindow After pid | Just pid <- [placementController a.placement]]

-- | If Amarthiúl leaves play, the players lose the game.
instance RunMessage AmarthiulTheTreacheryOfRhudaur where
  runMessage msg s@(AmarthiulTheTreacheryOfRhudaur a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged _ pid) <- windows]) $ takeControl a.id
      pure s
    Discard _ (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> AmarthiulTheTreacheryOfRhudaur <$> runMessage msg a
