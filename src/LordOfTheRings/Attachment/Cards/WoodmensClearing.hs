module LordOfTheRings.Attachment.Cards.WoodmensClearing (woodmensClearing) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype WoodmensClearing = WoodmensClearing AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a location. Limit 1 per location.
woodmensClearing :: AttachmentCard WoodmensClearing
woodmensClearing = attachment WoodmensClearing Cards.woodmensClearing & attachTo (AttachToLocation AnyLocation)

-- | Response: When attached location leaves play as an explored location, the first player
-- reduces his threat by 3.
instance HasAbilities WoodmensClearing where
  getAbilities (WoodmensClearing a) =
    [mkAbility a 1 $ response $ ExploredWindow When lid | AttachedTo (LocationTarget lid) <- [a.placement]]

instance RunMessage WoodmensClearing where
  runMessage msg s@(WoodmensClearing a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      lowerThreatBy a lead 3
      pure s
    _ -> WoodmensClearing <$> runMessage msg a
