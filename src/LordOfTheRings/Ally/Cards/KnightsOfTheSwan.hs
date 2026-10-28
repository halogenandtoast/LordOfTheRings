module LordOfTheRings.Ally.Cards.KnightsOfTheSwan (knightsOfTheSwan) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Outlands))

newtype KnightsOfTheSwan = KnightsOfTheSwan AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

knightsOfTheSwan :: AllyCard KnightsOfTheSwan
knightsOfTheSwan = ally KnightsOfTheSwan Cards.knightsOfTheSwan

-- | Each Outlands character you control gets +1 attack.
instance HasModifiersFor KnightsOfTheSwan where
  getModifiersFor (KnightsOfTheSwan a) = for_ (placementController a.placement) \pid -> do
    outlands <- controlledCharactersWithTrait pid Outlands
    for_ outlands \c -> modified a c [AttackModifier 1]
