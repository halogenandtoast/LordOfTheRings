module LordOfTheRings.Attachment.Cards.FriendOfFriends (friendOfFriends) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Trait (Trait (Hobbit))

newtype FriendOfFriends = FriendOfFriends AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit hero.
friendOfFriends :: AttachmentCard FriendOfFriends
friendOfFriends = attachment FriendOfFriends Cards.friendOfFriends & attachTo (AttachToHero $ HeroWithTrait Hobbit)

{- | Limit 2 copies of Friend of Friends in play: a third cannot be played from hand. While
another hero has Friend of Friends attached, attached hero gets +1 willpower, +1 attack,
+1 defense and +1 hit point.
-}
instance HasModifiersFor FriendOfFriends where
  getModifiersFor (FriendOfFriends a) = do
    copies <- getsGame (filter ((== toCardCode Cards.friendOfFriends) . toCardCode) . toList . entitiesAttachments . gameEntities)
    case a.placement of
      Unplaced -> when (length copies >= 2) $ modified a (CardIdTarget a.cardId) [CannotBePlayedFromHand]
      AttachedTo host@(HeroTarget _) -> do
        let otherHero c = case (toAttrs c).placement of
              AttachedTo h@(HeroTarget _) -> h /= host
              _ -> False
        when (any otherHero copies)
          $ modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1, HitPointModifier 1]
      _ -> pure ()
