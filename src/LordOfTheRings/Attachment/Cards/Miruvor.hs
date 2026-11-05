module LordOfTheRings.Attachment.Cards.Miruvor (miruvor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCard)

newtype Miruvor = Miruvor AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
miruvor :: AttachmentCard Miruvor
miruvor = attachment Miruvor Cards.miruvor & attachTo (AttachToHero AnyHero)

-- | Action: Discard Miruvor to (choose two): ready attached hero, add 1 resource to
-- attached hero's resource pool, attached hero gets +1 willpower until the end of the
-- round, or put Miruvor on the top of your deck.
instance HasAbilities Miruvor where
  getAbilities (Miruvor a) = [mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

instance RunMessage Miruvor where
  runMessage msg s@(Miruvor a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a \host -> do
        discard a a
        let effect :: ReverseQueue n => Int -> n ()
            effect = \case
              1 -> ready host
              2 -> placeResources a host 1
              3 -> roundModifier a host (WillpowerModifier 1)
              -- the deck of the player resolving it, its owner or not (ruling 3.027)
              _
                | pid == a.owner -> push $ PutDiscardOnTopOfDeck pid a.cardId
                | otherwise -> do
                    removeFromDiscard a.owner a.cardId
                    putOnTopOfDeck pid (toCard a)
            key :: Int -> Text
            key = \case
              1 -> "miruvor.ready"
              2 -> "miruvor.resource"
              3 -> "miruvor.willpower"
              _ -> "miruvor.topOfDeck"
        chooseOneM pid do
          questionLabeled "miruvor.chooseTwo"
          causedBy a
          for_ [1 .. 4] \i -> labeled (key i) do
            effect i
            chooseOneM pid do
              questionLabeled "miruvor.chooseAnother"
              causedBy a
              for_ (filter (/= i) [1 .. 4]) \j -> labeled (key j) (effect j)
      pure s
    _ -> Miruvor <$> runMessage msg a
