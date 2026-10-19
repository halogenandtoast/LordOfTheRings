module LordOfTheRings.Location.Cards.EnchantedStream (enchantedStream) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype EnchantedStream = EnchantedStream LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

enchantedStream :: LocationCard EnchantedStream
enchantedStream = location EnchantedStream Cards.enchantedStream

-- | While Enchanted Stream is the active location, players cannot draw cards.
instance HasModifiersFor EnchantedStream where
  getModifiersFor (EnchantedStream a) = when (a.placement == ActiveLocation) do
    order <- getPlayerOrder
    for_ order \p -> modified a (PlayerTarget p) [CannotDrawCards]
