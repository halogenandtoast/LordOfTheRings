module LordOfTheRings.Attachment.Cards.Arod (arod) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Arod = Arod AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero or Legolas.
arod :: AttachmentCard Arod
arod = attachment Arod Cards.arod & attachTo (AttachToCharacterMatching AnyHero (AllyWithTitle "Legolas"))

-- | Response: After attached character participates in an attack that destroys an enemy,
-- exhaust Arod to place 1 progress token on any location.
instance HasAbilities Arod where
  getAbilities (Arod a) =
    [ restricted a 1 (exists AnyLocation) $ ResponseAbility (EnemyDestroyedByAttackWindow After (Just host)) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage Arod where
  runMessage msg s@(Arod a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select AnyLocation
      chooseOrRunOneM pid do
        questionLabeled "arod.location"
        causedBy a
        targets locations \l -> placeProgress a l 1
      pure s
    _ -> Arod <$> runMessage msg a
