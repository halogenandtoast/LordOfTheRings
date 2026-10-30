module LordOfTheRings.Ally.Cards.VeteranOfOsgiliath (veteranOfOsgiliath) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype VeteranOfOsgiliath = VeteranOfOsgiliath AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

veteranOfOsgiliath :: AllyCard VeteranOfOsgiliath
veteranOfOsgiliath = ally VeteranOfOsgiliath Cards.veteranOfOsgiliath

-- | Veteran of Osgiliath gets +1 willpower, +1 attack and +1 defense while your threat is
-- 40 or higher.
instance HasModifiersFor VeteranOfOsgiliath where
  getModifiersFor (VeteranOfOsgiliath a) = for_ (placementController a.placement) \pid -> do
    high <- selectAny (PlayerWithId pid <> PlayerWithThreatAtLeast 40)
    when high $ modifySelf a [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
