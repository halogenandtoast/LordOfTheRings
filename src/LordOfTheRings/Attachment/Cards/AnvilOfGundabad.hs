module LordOfTheRings.Attachment.Cards.AnvilOfGundabad (anvilOfGundabad) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType), costAsResources)
import LordOfTheRings.Helpers.Attachment (playableAttachmentsInHand)
import LordOfTheRings.Trait (Trait (Item))

newtype AnvilOfGundabad = AnvilOfGundabad AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character.
anvilOfGundabad :: AttachmentCard AnvilOfGundabad
anvilOfGundabad = attachment AnvilOfGundabad Cards.anvilOfGundabad & attachTo AttachToCharacter

itemAttachment :: CardMatcher
itemAttachment = CardWithType AttachmentType <> CardWithTrait Item

{- | Response: After you claim or play Anvil of Gundabad, search the top 5 cards of your
deck for any number of Item attachments and add them to your hand. Shuffle your deck.
Action: Exhaust Anvil of Gundabad and attached character to play an Item attachment from
your hand, reducing its cost by 1.
-}
instance HasAbilities AnvilOfGundabad where
  getAbilities (AnvilOfGundabad a) =
    [mkAbility a 1 $ response $ AnyWindowOf [PlayedFromHandWindow After self, ClaimedWindow After self]]
      <> [ restricted a 2 (PlayableAttachmentInHand a.controller itemAttachment 1) $ ActionAbility (Costs [exhaustCost a, ExhaustCost host])
         | AttachedTo host <- [a.placement]
         ]
   where
    self = CardIdTarget a.cardId

instance RunMessage AnvilOfGundabad where
  runMessage msg s@(AnvilOfGundabad a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 itemAttachment True
      pure s
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      -- only an Item attachment with a legal host it could go on
      playable <- playableAttachmentsInHand pid itemAttachment 1
      let cost c = maybe 0 costAsResources c.cost
      chooseOneM pid do
        questionLabeled "anvilOfGundabad.item"
        causedBy a
        targets playable \c -> do
          when (cost c > 0) $ phaseModifier a (CardIdTarget c.id) (CostModifier (-1))
          playFromHand pid c
      pure s
    _ -> AnvilOfGundabad <$> runMessage msg a
