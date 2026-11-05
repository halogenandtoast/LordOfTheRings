module LordOfTheRings.Attachment.Cards.OrdersFromAngmar (ordersFromAngmar) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Trait (Trait (Undead))

newtype OrdersFromAngmar = OrdersFromAngmar AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from The Treachery of Rhudaur's Clue. Attach to
-- a hero. Restricted.
ordersFromAngmar :: AttachmentCard OrdersFromAngmar
ordersFromAngmar = attachment OrdersFromAngmar Cards.ordersFromAngmar

-- | Attached hero gets +1 defense (+2 defense instead while defending against an Undead
-- enemy).
instance HasModifiersFor OrdersFromAngmar where
  getModifiersFor (OrdersFromAngmar a) = case a.placement of
    AttachedTo host -> modified a host [DefenseModifier 1, DefenseBonusAgainst Undead 1]
    _ -> pure ()

-- | Response: After you play Orders from Angmar, discard a non-unique Undead enemy with 2 or
-- less remaining hit points.
instance HasAbilities OrdersFromAngmar where
  getAbilities (OrdersFromAngmar a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage OrdersFromAngmar where
  runMessage msg s@(OrdersFromAngmar a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      undead <- select (EnemyWithTrait Undead <> not_ UniqueEnemy)
      weak <- flip filterM undead \e -> (<= 2) <$> ((-) <$> getEnemyHitPoints e <*> getDamage (toTarget e))
      chooseOrRunOneM pid do
        questionLabeled "ordersFromAngmar.discard"
        causedBy a
        targets weak (discard a)
      pure s
    _ -> OrdersFromAngmar <$> runMessage msg a
