module LordOfTheRings.Ally.Cards.LongbeardMapMaker (longbeardMapMaker) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Lore))

newtype LongbeardMapMaker = LongbeardMapMaker AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

longbeardMapMaker :: AllyCard LongbeardMapMaker
longbeardMapMaker = ally LongbeardMapMaker Cards.longbeardMapMaker

-- | Action: Spend 1 Lore resource to give Longbeard Map-Maker +1 willpower until the end
-- of the phase.
instance HasAbilities LongbeardMapMaker where
  getAbilities (LongbeardMapMaker a) =
    [mkAbility a 1 $ ActionAbility (SphereResourceCost pid Lore 1) | Just pid <- [placementController a.placement]]

instance RunMessage LongbeardMapMaker where
  runMessage msg s@(LongbeardMapMaker a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      phaseModifier a a (WillpowerModifier 1)
      pure s
    _ -> LongbeardMapMaker <$> runMessage msg a
