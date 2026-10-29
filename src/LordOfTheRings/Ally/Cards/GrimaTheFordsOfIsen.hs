module LordOfTheRings.Ally.Cards.GrimaTheFordsOfIsen (grimaTheFordsOfIsen) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype GrimaTheFordsOfIsen = GrimaTheFordsOfIsen AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, once he is free of The Islet.
grimaTheFordsOfIsen :: AllyCard GrimaTheFordsOfIsen
grimaTheFordsOfIsen = ally GrimaTheFordsOfIsen Cards.grimaTheFordsOfIsen

-- | Action: Exhaust Gríma to draw a card.
instance HasAbilities GrimaTheFordsOfIsen where
  getAbilities (GrimaTheFordsOfIsen a) =
    [ mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

-- | If Gríma leaves play, the players lose the game: read off the window he leaves play in,
-- however he leaves.
instance RunMessage GrimaTheFordsOfIsen where
  runMessage msg s@(GrimaTheFordsOfIsen a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards pid 1
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    _ -> GrimaTheFordsOfIsen <$> runMessage msg a
