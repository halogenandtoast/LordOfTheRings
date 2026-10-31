module LordOfTheRings.Attachment.Cards.ParryingCutlass (parryingCutlass, parryingCutlassUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ParryingCutlass = ParryingCutlass AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Permanent. Restricted. A boon, chosen after Raid on the Grey Havens.
parryingCutlass :: AttachmentCard ParryingCutlass
parryingCutlass = attachment ParryingCutlass Cards.parryingCutlass

parryingCutlassUpgraded :: AttachmentCard ParryingCutlass
parryingCutlassUpgraded = attachment ParryingCutlass Cards.parryingCutlassUpgraded

-- | Attached hero gets +1 defense (+2 defense instead if it has 2 or more resources in its
-- resource pool). Upgraded, +2 (+3).
instance HasModifiersFor ParryingCutlass where
  getModifiersFor (ParryingCutlass a) = case a.placement of
    AttachedTo (HeroTarget hid) -> do
      h <- getHero hid
      let base = if isUpgradedSide a then 2 else 1
      modified a hid [DefenseModifier $ base + (if h.resources >= 2 then 1 else 0)]
    _ -> pure ()

-- | Upgraded: Response: When attached hero defends against an enemy, exhaust Parrying
-- Cutlass to move 1 resource from that enemy to attached hero's resource pool.
instance HasAbilities ParryingCutlass where
  getAbilities (ParryingCutlass a) =
    [ restricted a 1 (exists EnemyWithResources) $ ResponseAbility (DefenderDeclaredWindow After (Just host)) (exhaustCost a)
    | isUpgradedSide a
    , AttachedTo host <- [a.placement]
    ]

instance RunMessage ParryingCutlass where
  runMessage msg s@(ParryingCutlass a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      withHost a \host -> for_ [eid | Window _ (DeclaredAsDefender _ eid) <- windows] \eid -> do
        whenM (selectAny $ EnemyWithId eid <> EnemyWithResources) do
          removeTokens a eid Resource 1
          placeResources a host 1
      pure s
    _ -> ParryingCutlass <$> runMessage msg a
