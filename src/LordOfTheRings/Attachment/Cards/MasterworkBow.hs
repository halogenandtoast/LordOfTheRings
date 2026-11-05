module LordOfTheRings.Attachment.Cards.MasterworkBow (masterworkBow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype MasterworkBow = MasterworkBow AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character. Restricted, on its card def.
masterworkBow :: AttachmentCard MasterworkBow
masterworkBow = attachment MasterworkBow Cards.masterworkBow & attachTo AttachToCharacter

-- | Attached character gets +1 [attack] and gains ranged.
instance HasModifiersFor MasterworkBow where
  getModifiersFor (MasterworkBow a) = withHost a \host -> modified a host [AttackModifier 1, GainKeyword Ranged]

-- | Response: After attached character attacks and defeats an enemy, place 1 progress on a
-- location in the staging area.
instance HasAbilities MasterworkBow where
  getAbilities (MasterworkBow a) =
    [ restricted a 1 (exists LocationInStagingArea) $ response $ EnemyDestroyedByAttackWindow After (Just host)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage MasterworkBow where
  runMessage msg s@(MasterworkBow a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select LocationInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "chooseLocation"
        causedBy a
        targets locations \l -> placeProgress a l 1
      pure s
    _ -> MasterworkBow <$> runMessage msg a
