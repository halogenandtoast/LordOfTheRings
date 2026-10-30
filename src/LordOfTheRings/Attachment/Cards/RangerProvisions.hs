module LordOfTheRings.Attachment.Cards.RangerProvisions (rangerProvisions) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype RangerProvisions = RangerProvisions AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a location. Limit 1 per location, on its card def.
rangerProvisions :: AttachmentCard RangerProvisions
rangerProvisions = attachment RangerProvisions Cards.rangerProvisions & attachTo (AttachToLocation AnyLocation)

-- | Response: After attached location is explored, the first player adds 1 resource to
-- each of his heroes' resource pools.
instance HasAbilities RangerProvisions where
  getAbilities (RangerProvisions a) =
    [mkAbility a 1 $ response $ ExploredWindow When lid | AttachedTo (LocationTarget lid) <- [a.placement]]

instance RunMessage RangerProvisions where
  runMessage msg s@(RangerProvisions a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      heroes <- select (HeroControlledBy $ PlayerWithId lead)
      for_ heroes \h -> placeResources a h 1
      pure s
    _ -> RangerProvisions <$> runMessage msg a
