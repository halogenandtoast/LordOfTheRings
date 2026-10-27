module LordOfTheRings.Ally.Cards.ArwenUndomielObjective (
  arwenUndomielTheRedhornGate,
  arwenUndomielRoadToRivendell,
) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype ArwenUndomiel = ArwenUndomiel AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally The Redhorn Gate and Road to Rivendell share, under the first
-- player's control from the start.
arwenUndomielTheRedhornGate :: AllyCard ArwenUndomiel
arwenUndomielTheRedhornGate = ally ArwenUndomiel Cards.arwenUndomielTheRedhornGate

arwenUndomielRoadToRivendell :: AllyCard ArwenUndomiel
arwenUndomielRoadToRivendell = ally ArwenUndomiel Cards.arwenUndomielRoadToRivendell

-- | Response: After Arwen Undómiel exhausts, choose a hero. Add 1 resource to that hero's
-- resource pool.
instance HasAbilities ArwenUndomiel where
  getAbilities (ArwenUndomiel a) = [mkAbility a 1 $ response $ ExhaustedWindow After (toTarget a)]

-- | If Arwen Undómiel leaves play, the players are defeated.
instance RunMessage ArwenUndomiel where
  runMessage msg s@(ArwenUndomiel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroForResource"
        causedBy a
        targets heroes \h -> placeResources a h 1
      pure s
    Discard (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> ArwenUndomiel <$> runMessage msg a
