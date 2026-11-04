module LordOfTheRings.Attachment.Cards.OrcristTreasure (orcristTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcristTreasure = OrcristTreasure AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure Orcrist of We Must Away, Ere Break of Day. Attach to a hero. Restricted, on
-- its card def.
orcristTreasure :: AttachmentCard OrcristTreasure
orcristTreasure = attachment OrcristTreasure Cards.orcristTreasure

-- | Attached character gets +2 [attack].
instance HasModifiersFor OrcristTreasure where
  getModifiersFor (OrcristTreasure a) = withHost a \host -> modified a host [AttackModifier 2]

-- | Response: After attached hero destroys an Orc enemy, add 1 resource to that hero's
-- resource pool.
instance HasAbilities OrcristTreasure where
  getAbilities (OrcristTreasure a) =
    [mkAbility a 1 $ response $ EnemyWithTraitDestroyedByAttackWindow After Orc host | AttachedTo host <- [a.placement]]

instance RunMessage OrcristTreasure where
  runMessage msg s@(OrcristTreasure a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> OrcristTreasure <$> runMessage msg a
