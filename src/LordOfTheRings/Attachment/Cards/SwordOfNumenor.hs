module LordOfTheRings.Attachment.Cards.SwordOfNumenor (swordOfNumenor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain, Gondor))

newtype SwordOfNumenor = SwordOfNumenor AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dúnedain or Gondor hero. Restricted, on its card def.
swordOfNumenor :: AttachmentCard SwordOfNumenor
swordOfNumenor =
  attachment SwordOfNumenor Cards.swordOfNumenor
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Dunedain, HeroWithTrait Gondor])

-- | Attached hero gets +1 attack.
instance HasModifiersFor SwordOfNumenor where
  getModifiersFor (SwordOfNumenor a) = withHost a \host -> modified a host [AttackModifier 1]

-- | Response: After attached hero participates in an attack that destroys an enemy with 5
-- or more printed hit points, exhaust Sword of Númenor to add 1 resource to attached
-- hero's resource pool.
instance HasAbilities SwordOfNumenor where
  getAbilities (SwordOfNumenor a) =
    [ mkAbility a 1 $ ResponseAbility (EnemyWithPrintedHitPointsDestroyedByAttackWindow After 5 host) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage SwordOfNumenor where
  runMessage msg s@(SwordOfNumenor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> SwordOfNumenor <$> runMessage msg a
