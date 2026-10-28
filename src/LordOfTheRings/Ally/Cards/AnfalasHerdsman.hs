module LordOfTheRings.Ally.Cards.AnfalasHerdsman (anfalasHerdsman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Outlands))

newtype AnfalasHerdsman = AnfalasHerdsman AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

anfalasHerdsman :: AllyCard AnfalasHerdsman
anfalasHerdsman = ally AnfalasHerdsman Cards.anfalasHerdsman

-- | Each Outlands character you control gets +1 hit point.
instance HasModifiersFor AnfalasHerdsman where
  getModifiersFor (AnfalasHerdsman a) = for_ (placementController a.placement) \pid -> do
    outlands <- controlledCharactersWithTrait pid Outlands
    for_ outlands \c -> modified a c [HitPointModifier 1]
