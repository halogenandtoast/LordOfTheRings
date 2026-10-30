module LordOfTheRings.Ally.Cards.Amarthiul (amarthiul) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (placementController)

newtype Amarthiul = Amarthiul AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
amarthiul :: AllyCard Amarthiul
amarthiul = ally Amarthiul Cards.amarthiul

-- | Response: After an enemy engages a player, give control of Amarthiúl to that player.
-- If Amarthiúl leaves play, the players lose the game.
instance HasAbilities Amarthiul where
  getAbilities (Amarthiul a) =
    [ mkAbility a 1 $ response $ AnyEnemyEngagedWindow After
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage Amarthiul where
  runMessage msg s@(Amarthiul a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws, Just pid /= placementController a.placement] (takeControl a.id)
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    _ -> Amarthiul <$> runMessage msg a
