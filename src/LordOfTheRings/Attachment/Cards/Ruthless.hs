module LordOfTheRings.Attachment.Cards.Ruthless (ruthless) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Ruthless = Ruthless AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Mysterious Omens. Permanent.
ruthless :: AttachmentCard Ruthless
ruthless = attachment Ruthless Cards.ruthless

-- | Attached hero gets +1 attack.
instance HasModifiersFor Ruthless where
  getModifiersFor (Ruthless a) = case a.placement of
    AttachedTo host -> modified a host [AttackModifier 1]
    _ -> pure ()

-- | Action: Move any amount of damage from attached hero to an enemy engaged with you.
-- (Limit once per game.)
instance HasAbilities Ruthless where
  getAbilities (Ruthless a) = [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

instance RunMessage Ruthless where
  runMessage msg s@(Ruthless a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      withHost a \host -> do
        damage <- getDamage host
        chooseOrRunOneM pid do
          questionLabeled "ruthless.moveDamage"
          causedBy a
          targets enemies \e -> chooseAmountM pid "ruthless.amount" a 1 damage \n -> do
            removeTokens a host Damage n
            placeDamage a e n
      pure s
    _ -> Ruthless <$> runMessage msg a
