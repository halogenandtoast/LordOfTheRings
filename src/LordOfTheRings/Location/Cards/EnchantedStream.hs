module LordOfTheRings.Location.Cards.EnchantedStream (enchantedStream) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype EnchantedStream = EnchantedStream LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

enchantedStream :: LocationCard EnchantedStream
enchantedStream = location EnchantedStream Cards.enchantedStream
