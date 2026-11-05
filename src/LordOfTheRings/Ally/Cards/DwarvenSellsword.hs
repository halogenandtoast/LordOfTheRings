module LordOfTheRings.Ally.Cards.DwarvenSellsword (dwarvenSellsword) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype DwarvenSellsword = DwarvenSellsword AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dwarvenSellsword :: AllyCard DwarvenSellsword
dwarvenSellsword = ally DwarvenSellsword Cards.dwarvenSellsword

-- | Forced: At the end of the round, discard Dwarven Sellsword unless the players as a
-- group spend 1 leadership resource.
instance HasAbilities DwarvenSellsword where
  getAbilities (DwarvenSellsword a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage DwarvenSellsword where
  runMessage msg s@(DwarvenSellsword a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      groupMaySpendOr pid a Leadership 1 (pure ()) (discard a a)
      pure s
    _ -> DwarvenSellsword <$> runMessage msg a
