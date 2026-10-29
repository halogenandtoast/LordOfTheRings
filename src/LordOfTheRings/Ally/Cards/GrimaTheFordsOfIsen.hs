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
  getAbilities (GrimaTheFordsOfIsen a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

-- | If Gríma leaves play, the players lose the game.
-- ponytail: as Faramir's, only his discard (destroyed among it) is read as his leaving.
instance RunMessage GrimaTheFordsOfIsen where
  runMessage msg s@(GrimaTheFordsOfIsen a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards pid 1
      pure s
    Discard (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> GrimaTheFordsOfIsen <$> runMessage msg a
