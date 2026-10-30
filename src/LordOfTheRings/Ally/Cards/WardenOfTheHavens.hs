module LordOfTheRings.Ally.Cards.WardenOfTheHavens (wardenOfTheHavens) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Placement (placementController)

newtype WardenOfTheHavens = WardenOfTheHavens AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

wardenOfTheHavens :: AllyCard WardenOfTheHavens
wardenOfTheHavens = ally WardenOfTheHavens Cards.wardenOfTheHavens

-- | While the top card of your discard pile is an attachment, Warden of the Havens gets +2
-- defense and gains sentinel.
instance HasModifiersFor WardenOfTheHavens where
  getModifiersFor (WardenOfTheHavens a) = for_ (placementController a.placement) \pid ->
    whenM (topOfDiscardIs AttachmentType pid) $ modifySelf a [DefenseModifier 2, GainKeyword Sentinel]
