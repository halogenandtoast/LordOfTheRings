module LordOfTheRings.Attachment.Cards.Unbroken (unbroken) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Unbroken = Unbroken AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Accolade boon of the Ered Mithrin campaign. Permanent. Setup: Attach to a hero.
unbroken :: AttachmentCard Unbroken
unbroken = attachment Unbroken Cards.unbroken

-- | Attached hero gets +1 [defense].
instance HasModifiersFor Unbroken where
  getModifiersFor (Unbroken a) = withHost a \host -> modified a host [DefenseModifier 1]

{- | Action: Discard a card from your hand to heal 1 damage from attached hero. This
ability may be triggered by each player once each round. (The discard, its cost, is made
by whichever player triggers it, from his own hand.)
-}
instance HasAbilities Unbroken where
  getAbilities (Unbroken a) =
    [ anyPlayer
        $ limited (PlayerLimit PerRound 1)
        $ restricted a 1 (PlayerExists $ ActivePlayer <> PlayerWithCardsInHand 1)
        $ ActionAbility Free
    | AttachedTo (HeroTarget _) <- [a.placement]
    ]

instance RunMessage Unbroken where
  runMessage msg s@(Unbroken a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      chooseAndDiscardFromHand pid a 1
      withHost a \host -> healDamage a host 1
      pure s
    _ -> Unbroken <$> runMessage msg a
