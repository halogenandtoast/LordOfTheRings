module LordOfTheRings.Attachment.Cards.Hithlain (hithlain) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Silvan))

newtype Hithlain = Hithlain AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a location. Limit 1 per location, on its card def.
hithlain :: AttachmentCard Hithlain
hithlain = attachment Hithlain Cards.hithlain & attachTo (AttachToLocation AnyLocation)

-- | Response: After a player plays a Silvan ally from his hand, place 1 progress on
-- attached location.
instance HasAbilities Hithlain where
  getAbilities (Hithlain a) =
    [mkAbility a 1 $ response $ AnyPlayedCardFromHandWindow After (CardWithType AllyType <> CardWithTrait Silvan)]

instance RunMessage Hithlain where
  runMessage msg s@(Hithlain a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \l -> placeProgress a l 1
      pure s
    _ -> Hithlain <$> runMessage msg a
