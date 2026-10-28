module LordOfTheRings.Ally.Cards.HarborMaster (harborMaster) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype HarborMaster = HarborMaster AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

harborMaster :: AllyCard HarborMaster
harborMaster = ally HarborMaster Cards.harborMaster

-- | Response: After a card effect adds any number of resources to the resource pool of a
-- hero you control, Harbor Master gains +1 defense until the end of the round.
instance HasAbilities HarborMaster where
  getAbilities (HarborMaster a) =
    [mkAbility a 1 $ response $ HeroGainedResourcesWindow After pid | Just pid <- [placementController a.placement]]

instance RunMessage HarborMaster where
  runMessage msg s@(HarborMaster a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      roundModifier a a (DefenseModifier 1)
      pure s
    _ -> HarborMaster <$> runMessage msg a
