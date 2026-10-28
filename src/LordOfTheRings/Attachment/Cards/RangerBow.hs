module LordOfTheRings.Attachment.Cards.RangerBow (rangerBow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype RangerBow = RangerBow AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Ranger character. Restricted.
rangerBow :: AttachmentCard RangerBow
rangerBow =
  attachment RangerBow Cards.rangerBow
    & attachTo (AttachToCharacterMatching (HeroWithTrait Ranger) (AllyWithTrait Ranger))

-- | Action: Exhaust Ranger Bow and attached character to deal 1 damage to an enemy in the
-- staging area.
instance HasAbilities RangerBow where
  getAbilities (RangerBow a) =
    [ restricted a 1 (exists EnemyInStagingArea) $ ActionAbility (Costs [exhaustCost a, ExhaustCost host])
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage RangerBow where
  runMessage msg s@(RangerBow a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select EnemyInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "rangerBow.damage"
        causedBy a
        targets enemies \e -> dealDamage a e 1
      pure s
    _ -> RangerBow <$> runMessage msg a
