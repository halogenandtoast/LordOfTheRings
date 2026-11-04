module LordOfTheRings.Attachment.Cards.ThrowingAxe (throwingAxe, throwingAxeUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThrowingAxe = ThrowingAxe AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Permanent. Restricted. A boon, chosen after Raid on the Grey Havens.
throwingAxe :: AttachmentCard ThrowingAxe
throwingAxe = attachment ThrowingAxe Cards.throwingAxe

throwingAxeUpgraded :: AttachmentCard ThrowingAxe
throwingAxeUpgraded = attachment ThrowingAxe Cards.throwingAxeUpgraded

-- | Attached hero gets +1 attack (+2 attack instead if it has 2 or more damage on it).
-- Upgraded, +2 (+3).
instance HasModifiersFor ThrowingAxe where
  getModifiersFor (ThrowingAxe a) = case a.placement of
    AttachedTo (HeroTarget hid) -> do
      h <- getHero hid
      let base = if isUpgradedSide a then 2 else 1
      modified a hid [AttackModifier $ base + (if h.damage >= 2 then 1 else 0)]
    _ -> pure ()

-- | Upgraded: Action: Either deal 1 damage to an enemy, or toss Throwing Axe. If it travels
-- at least two feet across the playing area and stops on any enemies, deal 4 damage to
-- one of them. Reattach Throwing Axe to its hero. (Limit once per game.)
-- There is no table to toss it across here, so the player says where it landed: on an
-- enemy of their choice, dealt 4 damage, or on none.
instance HasAbilities ThrowingAxe where
  getAbilities (ThrowingAxe a) =
    [ limited (PlayerLimit PerGame 1) $ restricted a 1 (exists EnemyNotImmuneToPlayerCards) $ ActionAbility Free
    | isUpgradedSide a
    , isAttached a.placement
    ]
   where
    isAttached = \case
      AttachedTo _ -> True
      _ -> False

instance RunMessage ThrowingAxe where
  runMessage msg s@(ThrowingAxe a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select EnemyNotImmuneToPlayerCards
      chooseOneM pid do
        questionLabeled "throwingAxe.choose"
        causedBy a
        labeled "throwingAxe.one" $ chooseOneM pid do
          questionLabeled "throwingAxe.chooseEnemy"
          causedBy a
          targets enemies \e -> dealDamage a e 1
        labeled "throwingAxe.toss" $ chooseOneM pid do
          questionLabeled "throwingAxe.landed"
          causedBy a
          targets enemies \e -> dealDamage a e 4
          labeled "throwingAxe.missed" $ pure ()
      pure s
    _ -> ThrowingAxe <$> runMessage msg a
