module LordOfTheRings.Attachment.Cards.IarionsPendant (iarionsPendant) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype IarionsPendant = IarionsPendant AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign.
iarionsPendant :: AttachmentCard IarionsPendant
iarionsPendant = attachment IarionsPendant Cards.iarionsPendant & attachTo AttachToCharacter

-- | Attached character gets +1 willpower.
instance HasModifiersFor IarionsPendant where
  getModifiersFor (IarionsPendant a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 1]
    _ -> pure ()

-- | Response: Add Iârion's Pendant to the victory display to cancel the "when revealed"
-- effects of a card that was just revealed from the encounter deck, or to cancel a shadow
-- effect just triggered.
instance HasAbilities IarionsPendant where
  getAbilities (IarionsPendant a) =
    [ restricted a 1 whenRevealedCancelable $ response $ CardRevealedWindow When
    , restricted a 2 shadowEffectsCancelable $ response $ ShadowEffectWindow When
    ]

instance RunMessage IarionsPendant where
  runMessage msg s@(IarionsPendant a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      addToVictoryDisplay a
      for_ [t | Window _ (Revealed t) <- windows] (cancelWhenRevealed pid)
      pure s
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      addToVictoryDisplay a
      cancelShadowEffects windows
      pure s
    _ -> IarionsPendant <$> runMessage msg a
