module LordOfTheRings.Attachment.Cards.SparePipe (sparePipe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (EventType))

newtype SparePipe = SparePipe AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character.
sparePipe :: AttachmentCard SparePipe
sparePipe = attachment SparePipe Cards.sparePipe & attachTo AttachToCharacter

-- | Attached character gets +1 hit point.
instance HasModifiersFor SparePipe where
  getModifiersFor (SparePipe a) = withHost a \host -> modified a host [HitPointModifier 1]

-- | Response: After you play Spare Pipe, search the top 5 cards of your deck for an event
-- and add it to your hand. Shuffle your deck.
instance HasAbilities SparePipe where
  getAbilities (SparePipe a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage SparePipe where
  runMessage msg s@(SparePipe a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 (CardWithType EventType) False
      pure s
    _ -> SparePipe <$> runMessage msg a
