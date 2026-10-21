module LordOfTheRings.Attachment.Cards.RohanWarhorse (rohanWarhorse) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Trait (Trait (Rohan))

newtype RohanWarhorse = RohanWarhorse AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rohanWarhorse :: AttachmentCard RohanWarhorse
rohanWarhorse =
  attachment RohanWarhorse Cards.rohanWarhorse
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithSphere Tactics, HeroWithTrait Rohan])

-- | Response: After attached hero participates in an attack that destroys an enemy,
-- exhaust Rohan Warhorse to ready attached hero.
instance HasAbilities RohanWarhorse where
  getAbilities (RohanWarhorse a) =
    [ mkAbility a 1 $ ResponseAbility (EnemyDestroyedByAttackWindow After (Just host)) (ExhaustCost $ toTarget a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage RohanWarhorse where
  runMessage msg s@(RohanWarhorse a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> RohanWarhorse <$> runMessage msg a
