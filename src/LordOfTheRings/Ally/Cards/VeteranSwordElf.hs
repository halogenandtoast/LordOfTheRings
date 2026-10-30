module LordOfTheRings.Ally.Cards.VeteranSwordElf (veteranSwordElf) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Placement (placementController)

newtype VeteranSwordElf = VeteranSwordElf AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

veteranSwordElf :: AllyCard VeteranSwordElf
veteranSwordElf = ally VeteranSwordElf Cards.veteranSwordElf

-- | Veteran Sword-elf gets +1 attack and +1 defense for each copy of Veteran Sword-elf in
-- your discard pile.
instance HasModifiersFor VeteranSwordElf where
  getModifiersFor (VeteranSwordElf a) = do
    let you = fromMaybe a.allyOwner (placementController a.placement)
    n <- length . filter ((== toCardCode a) . toCardCode) . (.discard) <$> getPlayer you
    when (n > 0) $ modifySelf a [AttackModifier n, DefenseModifier n]

instance RunMessage VeteranSwordElf where
  runMessage msg (VeteranSwordElf a) = VeteranSwordElf <$> runMessage msg a
