module LordOfTheRings.Ally.Cards.EthirSwordsman (ethirSwordsman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Outlands))

newtype EthirSwordsman = EthirSwordsman AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ethirSwordsman :: AllyCard EthirSwordsman
ethirSwordsman = ally EthirSwordsman Cards.ethirSwordsman

-- | Each Outlands character you control gets +1 willpower.
instance HasModifiersFor EthirSwordsman where
  getModifiersFor (EthirSwordsman a) = for_ (placementController a.placement) \pid -> do
    outlands <- controlledCharactersWithTrait pid Outlands
    for_ outlands \c -> modified a c [WillpowerModifier 1]
