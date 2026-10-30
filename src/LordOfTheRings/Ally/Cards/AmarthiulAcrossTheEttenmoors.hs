module LordOfTheRings.Ally.Cards.AmarthiulAcrossTheEttenmoors (amarthiulAcrossTheEttenmoors) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (placementController)

newtype AmarthiulAcrossTheEttenmoors = AmarthiulAcrossTheEttenmoors AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
amarthiulAcrossTheEttenmoors :: AllyCard AmarthiulAcrossTheEttenmoors
amarthiulAcrossTheEttenmoors = ally AmarthiulAcrossTheEttenmoors Cards.amarthiulAcrossTheEttenmoors

-- | Response: After an enemy engages a player, give control of Amarthiúl to that player.
-- If Amarthiúl leaves play, the players lose the game.
instance HasAbilities AmarthiulAcrossTheEttenmoors where
  getAbilities (AmarthiulAcrossTheEttenmoors a) =
    [ mkAbility a 1 $ response $ AnyEnemyEngagedWindow After
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage AmarthiulAcrossTheEttenmoors where
  runMessage msg s@(AmarthiulAcrossTheEttenmoors a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (lastMay [pid | Window _ (EnemyEngaged _ pid) <- ws]) \pid ->
        when (placementController a.placement /= Just pid) $ takeControl a.id pid
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    _ -> AmarthiulAcrossTheEttenmoors <$> runMessage msg a
