module LordOfTheRings.Ally.Cards.GaladrielsHandmaiden (galadrielsHandmaiden) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GaladrielsHandmaiden = GaladrielsHandmaiden AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

galadrielsHandmaiden :: AllyCard GaladrielsHandmaiden
galadrielsHandmaiden = ally GaladrielsHandmaiden Cards.galadrielsHandmaiden

-- | Response: After Galadriel's Handmaiden enters play, choose a player. That player
-- reduces his threat by 1.
instance HasAbilities GaladrielsHandmaiden where
  getAbilities (GaladrielsHandmaiden a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage GaladrielsHandmaiden where
  runMessage msg s@(GaladrielsHandmaiden a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      choosePlayerM pid a \p -> lowerThreat p 1
      pure s
    _ -> GaladrielsHandmaiden <$> runMessage msg a
