module LordOfTheRings.Ally.Cards.EreborBattleMaster (ereborBattleMaster) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Dwarf))

newtype EreborBattleMaster = EreborBattleMaster AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborBattleMaster :: AllyCard EreborBattleMaster
ereborBattleMaster = ally EreborBattleMaster Cards.ereborBattleMaster

-- | Erebor Battle Master gets +1 attack for each other Dwarf ally you control. (Limit +4
-- attack.)
instance HasModifiersFor EreborBattleMaster where
  getModifiersFor (EreborBattleMaster a) = for_ (placementController a.placement) \pid -> do
    others <- select (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Dwarf <> not_ (AllyWithId a.id))
    modifySelf a [AttackModifier $ min 4 $ length others]
