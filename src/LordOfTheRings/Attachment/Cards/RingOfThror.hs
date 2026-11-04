module LordOfTheRings.Attachment.Cards.RingOfThror (ringOfThror) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AttachmentType))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype RingOfThror = RingOfThror AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy or location), on its card def. Attach to a Dwarf hero.
ringOfThror :: AttachmentCard RingOfThror
ringOfThror = attachment RingOfThror Cards.ringOfThror & attachTo (AttachToHero $ HeroWithTrait Dwarf)

-- | Action: Exhaust Ring of Thrór and discard the top card of your deck to ready attached
-- hero. If the discarded card is an attachment, put it into play.
instance HasAbilities RingOfThror where
  getAbilities (RingOfThror a) =
    [ restricted a 1 (PlayerExists $ PlayerWithId a.controller <> PlayerWithCardsInDeck 1) $ ActionAbility (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage RingOfThror where
  runMessage msg s@(RingOfThror a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- headMay . (.deck) <$> getPlayer pid
      discardTopOfDeck pid 1
      withHost a ready
      for_ top \case
        PlayerCard card | cardMatches (CardWithType AttachmentType) card -> do
          removeFromDiscard pid card.id
          putAttachmentIntoPlay pid card
        _ -> pure ()
      pure s
    _ -> RingOfThror <$> runMessage msg a
