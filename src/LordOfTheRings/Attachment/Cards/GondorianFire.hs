module LordOfTheRings.Attachment.Cards.GondorianFire (gondorianFire) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain, Gondor))

newtype GondorianFire = GondorianFire AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Gondor or Dúnedain hero.
gondorianFire :: AttachmentCard GondorianFire
gondorianFire =
  attachment GondorianFire Cards.gondorianFire
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Gondor, HeroWithTrait Dunedain])

-- | Action: Spend 1 resource from attached hero's resource pool to give attached hero +1
-- attack for each resource in its resource pool until the end of the phase. (Limit once
-- per phase.) The pool is counted once the cost is paid.
instance HasAbilities GondorianFire where
  getAbilities (GondorianFire a) =
    [ limited (PlayerLimit PerPhase 1) $ mkAbility a 1 $ ActionAbility (ResourceCost host 1)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage GondorianFire where
  runMessage msg s@(GondorianFire a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      case a.placement of
        AttachedTo host@(HeroTarget hid) -> do
          n <- (.resources) <$> getHero hid
          when (n > 0) $ phaseModifier a host (AttackModifier n)
        _ -> pure ()
      pure s
    _ -> GondorianFire <$> runMessage msg a
